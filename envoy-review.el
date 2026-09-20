;;; envoy-review.el --- Review what the agent did to a file  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nick

;; Author: Nick <nick@maderightsoftware.com>
;; Maintainer: Nick <nick@maderightsoftware.com>
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

;; The agent edits the file itself, on disk, which is the point of the
;; package and also the thing that has to be reviewable.  So a run takes
;; a copy of the file first, and afterwards shows the difference between
;; the copy and the file, and offers to put the copy back.
;;
;; That holds whether the run succeeded, failed or was interrupted.  It
;; has to: a run stopped partway can leave some of its edits on disk and
;; not others.  Measured, on a file of thirty lines with the agent asked
;; to rewrite each one separately: interrupted at six seconds the file
;; was untouched, and at twenty-five seconds eleven lines had already
;; been rewritten and stayed rewritten.  The file was intact both times,
;; because every edit is a whole write, so restoring the copy is all the
;; repair anyone needs.  But "cancel" cannot mean "nothing happened",
;; and a package that said so would be lying about the user's file.
;;
;; A diff is therefore the ordinary way a run ends, not the unhappy one.

;;; Code:

(require 'diff)
(require 'diff-mode)
(require 'subr-x)

(defcustom envoy-review-display 'diff
  "How to present what the agent changed.

`diff'  a diff buffer, accepted with \\<envoy-review-mode-map>\\[envoy-review-accept] \
and undone with \\[envoy-review-revert]
`none'  keep the change and say so in the echo area

`none' leaves no review step at all.  The change is already on disk by
then, so the only way back is `undo' in the buffer, and only if the file
is being visited."
  :type '(choice (const :tag "Diff buffer" diff)
                 (const :tag "Keep it, no review" none))
  :group 'envoy)

(defvar-local envoy-review--file nil
  "The file this review buffer is about.")

(defvar-local envoy-review--snapshot nil
  "Temporary file holding the reviewed file's contents before the run.")

(defvar-local envoy-review--iterate nil
  "Runs another pass, with an optional original snapshot.")

(defvar-local envoy-review--summary nil
  "What the agent said about its own work.")
(defvar-local envoy-review--pending nil
  "Non-nil when the source buffer still has unsaved changes.")
(defvar-local envoy-review--source-buffer nil
  "The source buffer that was visiting the file before this review.")
(defvar-local envoy-review--source-file nil
  "The file the source buffer was visiting when this review began.")
(defvar-local envoy-review--source-detached nil
  "Non-nil when this review detached its source buffer.")
(defvar-local envoy-review--iteration-state nil
  "Temporary file holding the state produced by an iteration.")
(defvar envoy-review--queue nil
  "Reviews waiting for an open review of the same file.")
(defvar envoy-review--iterations nil
  "Iterations waiting for their next review to be opened.")





(defvar envoy-review-mode-map
  ;; `make-sparse-keymap' and `define-key' rather than `defvar-keymap',
  ;; which arrived in Emacs 29 and this package supports 27.
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'envoy-review-accept)
    (define-key map (kbd "C-c C-k") #'envoy-review-revert)
    (define-key map (kbd "C-c C-i") #'envoy-review-iterate)
    map)
  "Keymap for `envoy-review-mode'.

Note what is deliberately shadowed.  In `diff-mode' the key that
`envoy-review-accept' takes here is `diff-goto-source', which is a
reasonable thing to want in this buffer and is still available, on
\\<envoy-review-mode-map>\\[envoy-review-goto-source].  Accepting and
undoing are what a person came to this buffer to do, so they get the keys
that are easiest to reach.")

(define-minor-mode envoy-review-mode
  "Minor mode for a diff of what a coding agent changed.

\\{envoy-review-mode-map}"
  :lighter " Envoy"
  :keymap envoy-review-mode-map)

(defun envoy-review-goto-source ()
  "Visit the source line this hunk refers to.
`diff-goto-source' under another key, because `envoy-review-mode' puts
`envoy-review-accept' on the key `diff-mode' gives it."
  (interactive)
  (call-interactively #'diff-goto-source))

(define-key envoy-review-mode-map (kbd "C-c C-v") #'envoy-review-goto-source)

(defun envoy-snapshot (file)
  "Copy FILE to a temporary file and return that file's name.
Returns nil when FILE does not exist yet, which is a run that will
create it and so has nothing to restore."
  (when (file-exists-p file)
    (let ((snapshot (make-temp-file "envoy-snapshot-" nil
                                    (concat "." (or (file-name-extension file)
                                                    "txt")))))
      (copy-file file snapshot t t)
      snapshot)))

(defun envoy--file-changed-p (file snapshot)
  "Return non-nil when FILE no longer matches SNAPSHOT."
  (or (and (null snapshot) (file-exists-p file))
      (and snapshot
           (or (not (file-exists-p file))
               (not (zerop (call-process diff-command nil nil nil
                                         "-q" snapshot file)))))))

(defun envoy--adopt-into-buffer (file &optional force)
  "Bring FILE's contents on disk into the buffer visiting it, if any.

Point, markers and overlays all survive, because the new text is merged
in rather than the buffer being emptied and refilled.  `revert-buffer'
would lose them, and moves point besides.

`clear-visited-file-modtime' first, and the reason is not cosmetic:
Emacs guards every buffer whose file changed underneath it, and
modifying such a buffer calls `ask-user-about-supersession-threat',
which prompts about clobbering someone else's work.  Here the change is
the agent's, and it is the change being adopted on purpose, so the guard
is telling the user about something they already know.  Clearing the
recorded time says so, and only at the point where that is true.

The merge itself is `replace-buffer-contents', which Emacs 31.1 marks
obsolete in favour of `replace-region-contents'.  The warning is
suppressed rather than the call being changed, because the two names do
not mean the same thing across the versions this package supports.
`replace-region-contents' exists as far back as the 27.1 floor, but
there it is Lisp taking (BEG END REPLACE-FN), a function that returns
the replacement; on 31.1 it is a C primitive taking (BEG END SOURCE),
where SOURCE is a buffer.  Either call would break half the matrix.
`replace-buffer-contents' itself is still defined on 31.1, as a shim
calling the new primitive, so the obsolete name works everywhere the
package runs.  The day it stops is the day this becomes a version
check.

When FORCE is non-nil, unsaved changes in the visiting buffer are
discarded without prompting."
  (let ((buffer (find-buffer-visiting file)))
    (if (not buffer)
        t
      (with-current-buffer buffer
        (if (and (buffer-modified-p)
                 (not force)
                 (not (y-or-n-p
                       (format "Envoy: discard unsaved changes in %s and adopt agent output? "
                               (file-name-nondirectory file)))))
            nil
          (if (not (file-exists-p file))
              (progn
                (set-visited-file-name nil t)
                t)
            (let ((inhibit-read-only t))
              (clear-visited-file-modtime)
              (with-temp-buffer
                (insert-file-contents file)
                (let ((source (current-buffer)))
                  (with-current-buffer buffer
                    (with-suppressed-warnings ((obsolete replace-buffer-contents))
                      (replace-buffer-contents source)))))
              (set-visited-file-modtime (file-attribute-modification-time
                                         (file-attributes file)))
              (set-buffer-modified-p nil)
              t)))))))

(defun envoy--diff-buffer-name (file)
  "Return the name of the review buffer for FILE."
  (format "*envoy: %s*" (file-truename file)))
(defun envoy--active-review-p (file)
  "Return non-nil when FILE already has an open review."
  (let ((existing (get-buffer (envoy--diff-buffer-name file))))
    (and existing
         (buffer-local-value 'envoy-review--file existing))))
(defun envoy-review--discard-queued ()
  "Discard queued reviews owned by the current review buffer."
  (let (remaining)
    (dolist (entry envoy-review--queue)
      (if (eq (nth 6 entry) (current-buffer))
          (dolist (snapshot (list (nth 1 entry) (nth 5 entry) (nth 7 entry)))
            (when (and (stringp snapshot) (file-exists-p snapshot))
              (delete-file snapshot)))
        (push entry remaining)))
    (setq envoy-review--queue (nreverse remaining))
    (when (and (stringp envoy-review--iteration-state)
               (file-exists-p envoy-review--iteration-state))
      (delete-file envoy-review--iteration-state))))

(defun envoy-review--remember-iteration (file snapshot)
  "Remember SNAPSHOT as a pending iteration for FILE."
  (push (list (file-truename file) snapshot) envoy-review--iterations))

(defun envoy-review--take-iteration (file snapshot)
  "Take and remove the pending iteration for FILE matching SNAPSHOT."
  (let ((canonical (file-truename file))
        entry)
    (dolist (candidate envoy-review--iterations)
      (when (and (not entry)
                 (equal canonical (nth 0 candidate))
                 (equal snapshot (nth 1 candidate)))
        (setq entry candidate)))
    (when entry
      (setq envoy-review--iterations (delq entry envoy-review--iterations)))
    entry))

(defun envoy--queue-review (file snapshot summary iterate &optional iteration-state)
  "Queue a review for FILE using SNAPSHOT, SUMMARY, and ITERATE.
ITERATION-STATE preserves iteration progress."
  (let ((owner (get-buffer (envoy--diff-buffer-name file)))
        (result-exists (file-exists-p file))
        (result-snapshot (envoy-snapshot file)))
    (setq envoy-review--queue
          (append envoy-review--queue
                  (list (list file snapshot summary iterate
                              result-exists result-snapshot owner
                              iteration-state))))))

(defun envoy--find-review (file)
  "Return the queued review entry for FILE, or nil."
  (let ((canonical (file-truename file))
        entry)
    (dolist (candidate envoy-review--queue)
      (when (and (not entry)
                 (equal canonical (file-truename (car candidate))))
        (setq entry candidate)))
    entry))

(defun envoy--dequeue-review (file)
  "Remove and return the queued review entry for FILE."
  (let ((entry (envoy--find-review file)))
    (when entry
      (setq envoy-review--queue (delq entry envoy-review--queue)))
    entry))

(defun envoy--show-next-review (file &optional preserved-state)
  "Show the next queued review for FILE, preserving PRESERVED-STATE."
  (let (review entry)
    (while (and (not review)
                (setq entry (envoy--find-review file)))
      (let ((queued-file (nth 0 entry))
            (snapshot (nth 1 entry))
            (summary (nth 2 entry))
            (iterate (nth 3 entry))
            (result-exists (nth 4 entry))
            (result-snapshot (nth 5 entry))
            (iteration-state (nth 7 entry)))
        (let (completed)
          (unwind-protect
              (progn
                (unless (and preserved-state
                             (if (eq preserved-state :missing)
                                 (not (file-exists-p file))
                               (not (envoy--file-changed-p file preserved-state))))
                  (if result-exists
                      (when result-snapshot
                        (copy-file result-snapshot queued-file t t))
                    (when (file-exists-p (file-truename queued-file))
                      (delete-file (file-truename queued-file)))))
                (setq review (envoy-review queued-file snapshot summary iterate
                                           iteration-state))
                (setq completed t
                      envoy-review--queue (delq entry envoy-review--queue)))
            (when completed
              (when (and result-snapshot (file-exists-p result-snapshot))
                (delete-file result-snapshot)))))))
    review))



(defun envoy--show-review (file snapshot summary iterate pending
                           &optional source-buffer source-file source-detached
                           iteration-state)
  "Open a review buffer for FILE against SNAPSHOT and describe SUMMARY.
ITERATE offers another pass when non-nil.  PENDING marks a review whose
source buffer was not adopted.  SOURCE-BUFFER identifies the source buffer
when it was not adopted.  SOURCE-FILE names the source file, and
SOURCE-DETACHED marks detached editing.  ITERATION-STATE carries saved
iteration state."
  (let ((buffer (get-buffer-create (envoy--diff-buffer-name file)))
        (diff-file file)
        (diff-snapshot snapshot)
        empty-file)
    (unwind-protect
        (progn
          (unless diff-snapshot
            (setq empty-file (make-temp-file "envoy-review-empty-"))
            (setq diff-snapshot empty-file))
          (unless (file-exists-p diff-file)
            (unless empty-file
              (setq empty-file (make-temp-file "envoy-review-empty-")))
            (setq diff-file empty-file))
          (diff-no-select diff-snapshot diff-file nil t buffer)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (insert (envoy--review-header file summary pending snapshot iterate)))
            (setq envoy-review--file file
                  envoy-review--snapshot snapshot
                  envoy-review--iterate iterate
                  envoy-review--summary summary
                  envoy-review--pending pending
                  envoy-review--source-buffer source-buffer
                  envoy-review--source-file source-file
                  envoy-review--source-detached source-detached
                  envoy-review--iteration-state iteration-state)
            (envoy-review-mode 1)
            (set-buffer-modified-p nil)
            (goto-char (point-min))
            (add-hook 'kill-buffer-hook #'envoy-review--discard-queued nil t))
          (pop-to-buffer buffer)
          buffer)
      (when empty-file (delete-file empty-file)))))

(defun envoy-review (file snapshot summary &optional iterate iteration-state)
  "Show what changed in FILE against SNAPSHOT and offer to keep or undo it.
SUMMARY is what the agent said about its work.  ITERATE, when given, is
a function of one argument, an instruction, and may accept a second
argument containing the original snapshot, to run the agent over FILE
again with a further instruction.  ITERATION-STATE is the file state
produced by an iteration.

Returns the review buffer, or nil when nothing changed."
  (let* ((iteration (and (not iteration-state)
                         (envoy-review--take-iteration file snapshot)))
         (iteration-file-state (or iteration-state
                                   (and iteration
                                        (if (file-exists-p file)
                                            (envoy-snapshot file)
                                          :missing)))))
    (if (envoy--active-review-p file)
        (if iterate
            (progn
              (envoy--queue-review file snapshot summary iterate
                                   iteration-file-state)
              nil)
          (when snapshot (delete-file snapshot))
          (when (and (stringp iteration-file-state)
                     (file-exists-p iteration-file-state))
            (delete-file iteration-file-state))
          (user-error "Envoy: a review for %s is already active"
                      (file-name-nondirectory file)))
      (let* ((source-buffer (find-buffer-visiting file))
             (source-file (and source-buffer
                               (buffer-local-value 'buffer-file-name source-buffer)))
             (adopted (envoy--adopt-into-buffer file))
             (source-detached (and source-buffer source-file
                                   (not (buffer-local-value
                                         'buffer-file-name source-buffer)))))
        (if (not adopted)
            (envoy--show-review file snapshot summary iterate t source-buffer
                                source-file source-detached iteration-file-state)
          (cond
           ((not (envoy--file-changed-p file snapshot))
            (message "Envoy: %s is unchanged%s"
                     (file-name-nondirectory file)
                     (if (string-empty-p (or summary "")) ""
                       (concat " — " summary)))
            (when snapshot (delete-file snapshot))
            (when (and (stringp iteration-file-state)
                       (file-exists-p iteration-file-state))
              (delete-file iteration-file-state))
            nil)
           ((eq envoy-review-display 'none)
            (message "Envoy: %s changed%s"
                     (file-name-nondirectory file)
                     (if (string-empty-p (or summary "")) ""
                       (concat " — " summary)))
            (when snapshot (delete-file snapshot))
            (when (and (stringp iteration-file-state)
                       (file-exists-p iteration-file-state))
              (delete-file iteration-file-state))
            nil)
           (t
            (envoy--show-review file snapshot summary iterate nil source-buffer
                                source-file source-detached iteration-file-state))))))))

(defun envoy--review-header (file summary &optional pending snapshot iterate)
  "Return the lines to put above the diff of FILE, describing SUMMARY.
PENDING indicates unsaved source changes.  SNAPSHOT enables undo, and ITERATE
enables another pass."
  (concat
   (format "# %s, already written to disk.\n" (file-name-nondirectory file))
   (if pending
       "# The source buffer has unsaved changes; the disk edit still needs review.\n"
     "")
   (if (string-empty-p (or summary ""))
       ""
     (concat "# " (string-join (split-string (string-trim summary) "\n" t)
                               "\n# ")
             "\n"))
   (if iterate
       (if snapshot
           "# C-c C-c keep it   C-c C-k undo it   C-c C-i another pass"
         "# C-c C-c keep it   C-c C-i another pass")
     (if snapshot
         "# C-c C-c keep it   C-c C-k undo it"
       "# C-c C-c keep it"))
   (if (file-exists-p file)
       "   C-c C-v go to source"
     "")
   "\n"))

(defun envoy-review--finish (message &optional keep-iteration-state)
  "Discard the review buffer's snapshot and close it, then say MESSAGE.
KEEP-ITERATION-STATE preserves the next iteration state when non-nil."
  (let* ((file envoy-review--file)
         (buffer (current-buffer))
         (snapshot envoy-review--snapshot)
         (iteration-state envoy-review--iteration-state)
         (preserved-state
          (and keep-iteration-state
               (or iteration-state
                   (if (file-exists-p file)
                       (or (envoy-snapshot file) :missing)
                     :missing)))))
    (quit-window)
    (with-current-buffer buffer
      (remove-hook 'kill-buffer-hook #'envoy-review--discard-queued t))
    (kill-buffer buffer)
    (unwind-protect
        (progn
          (envoy--show-next-review file preserved-state)
          (when (and snapshot (file-exists-p snapshot))
            (delete-file snapshot)))
      (when (and (stringp preserved-state)
                 (not (equal preserved-state iteration-state))
                 (file-exists-p preserved-state))
        (delete-file preserved-state))
      (when (and (stringp iteration-state)
                 (file-exists-p iteration-state))
        (delete-file iteration-state))))
  (message "%s" message))

(defun envoy-review-accept ()
  "Keep the agent's change and close the review."
  (interactive)
  (unless envoy-review--file
    (user-error "Envoy: not a review buffer"))
  (when (and envoy-review--pending
             (not (envoy--adopt-into-buffer envoy-review--file)))
    (user-error "Envoy: source buffer still has unsaved changes"))
  (envoy-review--finish (format "Envoy: kept the change to %s"
                                (file-name-nondirectory envoy-review--file))
                        t))

(defun envoy-review-revert ()
  "Put the file back as it was before the run, and close the review."
  (interactive)
  (unless envoy-review--file
    (user-error "Envoy: not a review buffer"))
  (let ((file envoy-review--file)
        (source-buffer envoy-review--source-buffer)
        (source-file envoy-review--source-file)
        (source-detached envoy-review--source-detached))
    (if envoy-review--snapshot
        (progn
          (unless (file-exists-p envoy-review--snapshot)
            (user-error "Envoy: the copy of %s taken before the run is gone"
                        (file-name-nondirectory envoy-review--file)))
          (copy-file envoy-review--snapshot file t t)
          (when (and (buffer-live-p source-buffer)
                     (with-current-buffer source-buffer
                       (let ((current-file (buffer-file-name)))
                         (or (and current-file source-file
                                  (equal (file-truename current-file)
                                         (file-truename source-file)))
                             (and source-detached (not current-file))))))
            (with-current-buffer source-buffer
              (set-visited-file-name file t t)))
          (envoy--adopt-into-buffer
           file
           (and source-detached
                (buffer-live-p source-buffer)
                (not (buffer-modified-p source-buffer)))))
      (when (file-exists-p file)
        (delete-file file))
      (when (and (buffer-live-p source-buffer)
                 (with-current-buffer source-buffer
                   (and (not (buffer-modified-p))
                        (let ((current-file (buffer-file-name)))
                          (and current-file source-file
                               (equal (file-truename current-file)
                                      (file-truename source-file)))))))
        (with-current-buffer source-buffer
          (set-visited-file-name nil t))))
    (envoy-review--finish (format "Envoy: put %s back"
                                  (file-name-nondirectory file)))))

(defun envoy-review-iterate (instruction)
  "Run the agent over the file again, with a further INSTRUCTION.
The change under review stays on disk and the agent works from it, so
this is another pass rather than a second attempt at the first one.  The
copy taken before the first run is kept, so undoing still goes all the
way back."
  (interactive
   (list (read-string "What else should it do? ")))
  (unless envoy-review--iterate
    (user-error "Envoy: this review cannot be iterated"))
  (when (string-empty-p (string-trim instruction))
    (user-error "Envoy: no instruction given"))
  (let ((iterate envoy-review--iterate)
        (snapshot envoy-review--snapshot)
        (buffer (current-buffer))
        (file envoy-review--file))
    (quit-window)
    (with-current-buffer buffer
      (remove-hook 'kill-buffer-hook #'envoy-review--discard-queued t))
    (kill-buffer buffer)
    (envoy-review--remember-iteration file snapshot)
    (let ((arity (func-arity iterate)))
      (if (or (eq (cdr arity) 'many)
              (> (cdr arity) 1))
          (funcall iterate instruction snapshot)
        (funcall iterate instruction)))))

(provide 'envoy-review)

;;; envoy-review.el ends here
