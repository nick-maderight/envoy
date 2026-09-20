;;; envoy.el --- Delegate editing to a coding agent  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nick

;; Author: Nick <nick@maderightsoftware.com>
;; Maintainer: Nick <nick@maderightsoftware.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools, processes
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://github.com/nick-maderight/envoy

;; This file is part of envoy.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Mark a region, say what you want done to it, and a coding agent does
;; it.  Not a language model: Claude Code or reasonix, the same program
;; you would run in a terminal, run here without a terminal in the way.
;;
;; The difference is where the work happens.  An Emacs package talking to
;; a model has to do the agent's job itself — decide which files are
;; relevant, read them, assemble them into a prompt inside a token
;; budget, get a block of text back, and splice it into the buffer.  All
;; of that is control flow, and none of it is Emacs's to write.  An agent
;; already has it, and better than a package will: pointed at a file and
;; a range of lines, it reads its own way around the project, opens what
;; it needs, and edits the file.
;;
;; So envoy sends a small, honest brief and gets out of the way.  What is
;; left for Emacs is the part Emacs is good at: which region, which
;; instruction, and what the change looks like before you keep it.
;;
;; The agent writes to the file on disk.  Every run copies the file
;; first, and ends by showing you the difference between the copy and the
;; file:
;;
;;   C-c C-c   keep it
;;   C-c C-k   put it back
;;   C-c C-i   another pass
;;
;; That is also true of a run you interrupt, because an interrupted agent
;; can leave some of its edits behind.  See envoy-review.el.
;;
;; Usage:
;;
;;   M-x envoy-rewrite        rewrite the region
;;   M-x envoy-cancel         stop the run
;;   M-x envoy-select-provider
;;                            switch between Claude Code and reasonix
;;
;; And from org-mode, in envoy-org.el:
;;
;;   M-x envoy-org-heading    hand the heading at point over as a task

;;; Code:

(require 'envoy-process)
(require 'envoy-review)
(require 'subr-x)
(when (boundp 'envoy-rewrite-instructions)
  (makunbound 'envoy-rewrite-instructions))

(defcustom envoy-rewrite-context-lines 3
  "Number of complete context lines to include on each side of a rewrite.
The selected lines are always included in full.  A non-positive value
disables surrounding context."
  :type 'integer
  :group 'envoy)
(defcustom envoy-rewrite-extra-instructions nil
  "Extra instructions to add to every rewrite brief, or nil.
The value is appended verbatim after the instruction for the current
rewrite pass.  Whitespace-only values are ignored."
  :type '(choice (const :tag "None" nil) string)
  :group 'envoy)

(defvar envoy--runs nil
  "Runs that have not finished, as an alist of (BUFFER . PROCESS).")

(defun envoy--forget-run (buffer)
  "Drop BUFFER's entry from `envoy--runs'."
  (setq envoy--runs (assq-delete-all buffer envoy--runs)))

(defun envoy--remember-run (buffer process)
  "Record PROCESS as BUFFER's run."
  (envoy--forget-run buffer)
  (push (cons buffer process) envoy--runs))

;;;###autoload
(defun envoy-cancel ()
  "Stop this buffer's run, or the most recent one if this buffer has none.

Whatever the agent has already written stays written; the review that
follows shows it, and can put the file back.  Interrupting an agent does
not undo the edits it finished, so this cannot promise nothing happened."
  (interactive)
  (let* ((entry (or (assq (current-buffer) envoy--runs)
                    (car envoy--runs)))
         (process (cdr entry)))
    (unless (and process (process-live-p process))
      (user-error "Envoy: nothing running"))
    (envoy-cancel-process process)
    (message "Envoy: stopping — the review will show what it got done")))

(defun envoy--live-run-p (buffer)
  "Return non-nil when BUFFER already has a run going."
  (let ((process (cdr (assq buffer envoy--runs))))
    (and process (process-live-p process))))

(defun envoy--project-directory (file)
  "Return the directory to run an agent in for FILE.

The agent treats this as the project, so it decides how far the agent
can see.  The version-safe route to it is not obvious: `project-root'
arrived in Emacs 28 and the 27 releases have only the obsolete
`project-roots', and `project-current' itself returns a different shape
depending on version and backend, so its value is passed along rather
than taken apart."
  (let ((default-directory (file-name-directory file)))
    (or (when (fboundp 'project-current)
          ;; when-let* rather than when-let: the latter is obsolete as of
          ;; Emacs 31.1, and the starred form goes back well past the 27.1
          ;; floor, so this is the one spelling that compiles clean on every
          ;; version the package supports.
          (when-let* ((project (project-current nil)))
            (cond
             ((fboundp 'project-root) (project-root project))
             ((fboundp 'project-roots) (car (project-roots project))))))
        (when (fboundp 'vc-root-dir) (vc-root-dir))
        (locate-dominating-file default-directory ".git")
        default-directory)))

(defun envoy--ensure-saved (buffer)
  "Make sure BUFFER's file on disk matches BUFFER, and return its file.

The agent reads the file, not the buffer, so unsaved changes would be
invisible to it and its edit would land on top of a version of the file
the user is not looking at."
  (with-current-buffer buffer
    (let ((file (buffer-file-name)))
      (unless file
        (user-error "Envoy: %s is not visiting a file, and the agent reads files"
                    (buffer-name)))
      (when (buffer-modified-p)
        (if (y-or-n-p (format "The agent reads the file, not the buffer; save %s first? "
                              (file-name-nondirectory file)))
            (save-buffer)
          (user-error "Envoy: %s has unsaved changes"
                      (file-name-nondirectory file))))
      file)))

(defun envoy--report (result)
  "Return a one-line account of RESULT for the echo area."
  (let ((cost (envoy-result-cost result))
        (turns (envoy-result-turns result)))
    (concat (envoy-provider-name)
            (when turns (format ", %s turn%s" turns (if (= turns 1) "" "s")))
            (when (and cost (> cost 0)) (format ", $%.2f" cost)))))

(defun envoy-delegate (prompt file &rest keys)
  "Send PROMPT to the agent, let it edit FILE, then show what it did.

Return the process.  KEYS are keyword arguments:

:iterate    function of one argument, an instruction, optionally accepting
            a second argument containing the original snapshot, called to
            run again from `envoy-review-iterate'
:snapshot   existing snapshot to use for this review and later iterations
:directory  where to run, defaulting to FILE's project
:buffer     the buffer the run belongs to, for `envoy-cancel'
:announce   what to say in the echo area while the agent works"
  (let* ((buffer (or (plist-get keys :buffer) (current-buffer)))
         (snapshot (if (memq :snapshot keys)
                       (plist-get keys :snapshot)
                     (envoy-snapshot file)))
         (directory (or (plist-get keys :directory)
                        (envoy--project-directory file)))
         (iterate (plist-get keys :iterate))
         process)
    (setq process
          (envoy-run
           prompt
           (lambda (result)
             (envoy--forget-run buffer)
             (unless (envoy-result-ok result)
               (message "Envoy: %s" (envoy-result-text result)))
             (let ((review
                    (envoy-review file snapshot
                                  (if (envoy-result-ok result)
                                      (envoy-result-text result)
                                    (envoy-result-text result))
                                  iterate)))
               (if (and (buffer-live-p review)
                        (buffer-local-value 'envoy-review--pending review))
                   (message "Envoy: %s — disk edit still needs review"
                            (envoy--report result))
                 (when (envoy-result-ok result)
                   (message "Envoy: %s" (envoy--report result))))))
           :edit t
           :directory directory))
    (when (and (processp process) (process-live-p process))
      (envoy--remember-run buffer process)
      (message "%s" (or (plist-get keys :announce)
                        (format "Envoy: %s is working on %s — C-g does nothing, M-x envoy-cancel stops it"
                                (envoy-provider-name)
                                (file-name-nondirectory file)))))
    process))

(defun envoy--rewrite-excerpt (file beg-line end-line)
  "Return surrounding and selected text from FILE as three strings.
BEG-LINE through END-LINE are selected; BEFORE and AFTER contain
complete lines bounded independently by `envoy-rewrite-context-lines'."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (forward-line (1- beg-line))
    (let ((selected-start (line-beginning-position)))
      (forward-line (1+ (- end-line beg-line)))
      (let* ((selected-end (point))
             (context-lines (max 0 envoy-rewrite-context-lines))
             (before-start
              (progn
                (goto-char selected-start)
                (forward-line (- context-lines))
                (point)))
             (after-end
              (progn
                (goto-char selected-end)
                (forward-line context-lines)
                (point))))
        (list (buffer-substring-no-properties before-start selected-start)
              (buffer-substring-no-properties selected-start selected-end)
              (buffer-substring-no-properties selected-end after-end))))))

(defun envoy--rewrite-prompt (file beg-line end-line instruction)
  "Return the brief for rewriting FILE between BEG-LINE and END-LINE.
INSTRUCTION is what to do.  The selected lines are marked inside a
bounded read-only excerpt from FILE so the agent can match local context.
The numbered range remains the only part the agent may change."
  (let* ((excerpt (envoy--rewrite-excerpt file beg-line end-line))
         (before (nth 0 excerpt))
         (selected (nth 1 excerpt))
         (after (nth 2 excerpt)))
    (concat
     (format "Rewrite lines %d to %d of %s.\n\n" beg-line end-line file)
     instruction "\n\n"
     (when (and envoy-rewrite-extra-instructions
                (not (string-empty-p
                      (string-trim envoy-rewrite-extra-instructions))))
       (concat envoy-rewrite-extra-instructions "\n\n"))
     "How to go about it:\n"
     (format "- Edit %s in place.  Do not write a new file and do not write a copy.\n" file)
     (format "- Change only lines %d to %d.  Leave every other line of the file exactly as it is.\n"
             beg-line end-line)
     "- Read whatever else you need to get this right: the rest of this file, its neighbours, the project's conventions.  That is the point of asking you rather than a model.\n"
     "- Follow the conventions already in the file over any general preference of your own.\n"
     "- When you are done, reply with one sentence saying what you changed.  No preamble, no file listing, no code fence.\n\n"
     "FILE CONTEXT — text outside the selection markers is read-only.  Do not rewrite or repeat it.  Use it only to match style, naming, and continuity.\n"
     "--- BEGIN FILE EXCERPT ---\n"
     before
     (unless (string-suffix-p "\n" before) "\n")
     "<<< SELECTION TO REWRITE STARTS >>>\n"
     selected
     (unless (string-suffix-p "\n" selected) "\n")
     "<<< SELECTION TO REWRITE ENDS >>>\n"
     after
     (unless (string-suffix-p "\n" after) "\n")
     "--- END FILE EXCERPT ---\n")))

(defun envoy--read-instruction (prompt)
  "Read a free-form instruction from the minibuffer, PROMPT for it."
  (let ((answer (read-string prompt)))
    (when (string-empty-p (string-trim answer))
      (user-error "Envoy: no instruction given"))
    answer))

;;;###autoload
(defun envoy-rewrite (beg end instruction)
  "Have a coding agent rewrite the region between BEG and END.
INSTRUCTION is what to do with it, read from the minibuffer.

The agent is given the file and the line numbers, not the text alone, so
it can read the rest of the file and the rest of the project before it
decides what the passage should say.  It edits the file itself.  When it
is done, the change is shown as a diff that
\\<envoy-review-mode-map>\\[envoy-review-accept] keeps and \
\\[envoy-review-revert] undoes.

The buffer has to be saved first, because the agent reads the file."
  (interactive
   (progn
     (unless (use-region-p)
       (user-error "Envoy: mark the text to rewrite first"))
     (list (region-beginning) (region-end)
           (envoy--read-instruction "Rewrite it how? "))))
  (when (envoy--live-run-p (current-buffer))
    (user-error "Envoy: this buffer already has a run going; M-x envoy-cancel first"))
  (when (and (buffer-file-name)
             (envoy--active-review-p (buffer-file-name)))
    (user-error "Envoy: a review for %s is already active"
                (file-name-nondirectory (buffer-file-name))))
  (unless (envoy-can-deny-tools-p)
    (unless (y-or-n-p
             (format "%s cannot be told not to run shell commands; continue? "
                     (envoy-provider-name)))
      (user-error "Envoy: stopped")))
  (let* ((buffer (current-buffer))
         (file (envoy--ensure-saved buffer))
         (beg-line (line-number-at-pos beg))
         ;; A region ending at the start of a line covers the newline
         ;; before it, not the line itself, and reporting that line as
         ;; part of the range would invite the agent to rewrite a line
         ;; the user did not mark.
         (end-line (save-excursion
                     (goto-char end)
                     (if (and (bolp) (> end beg))
                         (line-number-at-pos (1- end))
                       (line-number-at-pos end)))))
    (deactivate-mark)
    (let ((iterate nil))
      (setq iterate
            (lambda (further snapshot)
              (with-current-buffer buffer
                (envoy-delegate
                 (envoy--rewrite-prompt file beg-line end-line further)
                 file
                 :buffer buffer
                 :snapshot snapshot
                 :iterate iterate))))
      (envoy-delegate
       (envoy--rewrite-prompt file beg-line end-line instruction)
       file
       :buffer buffer
       :iterate iterate
       :announce (format "Envoy: %s is rewriting lines %d-%d of %s — M-x envoy-cancel stops it"
                         (envoy-provider-name) beg-line end-line
                         (file-name-nondirectory file))))))


;;;###autoload
(defun envoy-select-provider (provider)
  "Send later work to PROVIDER instead.
Changes `envoy-provider' for this session only; set it in your
configuration to make it stick."
  (interactive
   (list (intern (completing-read
                  (format "Agent (now %s): " (envoy-provider-name))
                  (mapcar (lambda (entry) (symbol-name (car entry)))
                          envoy-providers)
                  nil t))))
  (setq envoy-provider provider)
  (message "Envoy: %s%s"
           (envoy-provider-name)
           (if (envoy-can-deny-tools-p)
               ""
             ", which cannot be told not to run shell commands")))

(provide 'envoy)

;;; envoy.el ends here
