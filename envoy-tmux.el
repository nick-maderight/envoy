;;; envoy-tmux.el --- Hand a task to an agent in a tmux window  -*- lexical-binding: t; -*-

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

;; The other half of `envoy-org-heading'.  Same brief, different delivery:
;; instead of one non-interactive run whose summary is filed under the
;; heading, the agent is started in a tmux window with the brief already
;; typed in, and you can attach to it, read what it is doing and answer
;; it.  Long tasks are the case for this one, and tasks where the agent
;; will want to ask something.
;;
;; The session is asked for on every dispatch from the live list tmux reports.
;; The list is fetched for every dispatch rather than cached.  A session
;; started or killed in a terminal between two dispatches is the ordinary case.
;; Nothing chooses one for you.

;; Sessions are matched exactly, with tmux's `=' prefix.  Without it a
;; target is a session id, then an exact name, then a prefix of a name,
;; then a glob, so a session named `project-work' answers to `project'.
;; If two sessions share a prefix, the target is ambiguous and the
;; command fails.

;; A name absent from the live list starts a new session.
;;
;; The pane is never read.  What the agent says belongs to the terminal,
;; and a package that scraped the pane to guess when it had finished would
;; be wrong the first time the agent stopped to ask something.  That also
;; makes this the only transport an agent with no non-interactive mode can
;; use.  The pane is asked one thing, and only for agents whose prompt has
;; to be typed at them: which command is running in it, so that the typing
;; waits until the agent rather than the shell is reading.
;;
;; The heading is still written back to, and the agent is what closes that
;; loop rather than anything watching the terminal.  The brief names a file
;; for it to write its report to, and the name it uses says whether the
;; work is finished; `envoy-org-collect-reports' files what it finds under
;; the heading the id belongs to.  A report survives Emacs being restarted
;; while the agent is still working, which is the ordinary case for a task
;; long enough to have been sent here in the first place.

;;; Code:

(require 'envoy-process)
(require 'envoy-org)
(require 'org)
(require 'seq)
(require 'subr-x)

(defcustom envoy-tmux-program "tmux"
  "The tmux executable, looked up on the variable `exec-path'."
  :type 'string
  :group 'envoy)

(defcustom envoy-tmux-provider 'claude
  "Agent `envoy-tmux-org-heading' starts when it is not told which.
One of the keys of `envoy-tmux-providers'."
  :type 'symbol
  :group 'envoy)

(defcustom envoy-tmux-providers
  '((claude
     :program "claude"
     :name "Claude Code"
     :args ()
     :prompt-arg nil
     :model-arg "--model"
     :variable setup
     :setup nil
     :setups ())
    (reasonix
     :program "reasonix"
     :name "reasonix"
     :args ()
     :prompt-arg nil
     :model-arg "--model"
     :variable model
     :setup nil
     :models ())
    (pi
     :program "pi"
     :name "pi"
     :args ()
     :prompt-arg "@%s"
     :model-arg "--model"
     :variable setup
     :setup nil
     :setups ())
    (codex
     :program "codex"
     :name "Codex"
     ;; Not a preference.  Codex reads how fast typed input arrives and takes a
     ;; fast run of it as a paste, which swallows the Enter that follows; a
     ;; brief sent in pieces then sits in the composer unsent.  See
     ;; `envoy-tmux-type'.
     :args ("-c" "disable_paste_burst=true")
     ;; A prompt on codex's command line is submitted the moment the terminal
     ;; is up, which leaves no turn in which to choose a mode.  Typed instead,
     ;; so that :opening-input has somewhere to go.
     :prompt-arg nil
     :model-arg "--model"
     :variable setup
     :setup nil
     :setups ()
     ;; The empty line is an Enter alone, and it is delivery rather than
     ;; preference: see `envoy-tmux-opening-input'.  A mode goes after it.
     :opening-input (""))
    (omp
     :program "omp"
     :name "OMP"
     :args ()
     :prompt-arg "@%s"
     :model-arg "--model"
     :variable model
     :setup nil
     :models ()))
  "Agents envoy knows how to start in a tmux window.
Each entry is (SYMBOL . PLIST).  The properties are:

:program     the executable, looked up on the variable `exec-path'
:name        how to say it in a message to the user
:args        arguments to start it with
:prompt-arg  how the brief reaches it, as a format string taking the
             name of the file the brief was written to: \"@%s\" for an
             agent that reads a file named on its command line,
             \"\\\"$(cat %s)\\\"\" for one that takes the prompt itself
             as an argument.  nil for an agent that takes neither, and
             the brief is typed into it after it starts
:model-arg   option that selects a model
:variable    what a prefix argument asks for, `model' or `setup'
:setup       shell command to run before the agent, or nil.  This is
             where a provider-selecting wrapper goes
:models      models to offer when :variable is `model'
:setups      setup commands to offer when :variable is `setup'
:opening-input
             lines to type at the agent and submit before the brief,
             for something its command line cannot ask for.  An empty
             line is an Enter alone.  Only for an agent whose
             :prompt-arg is nil: where the prompt travels on the
             command line the agent has already been given its work by
             the time anything could be typed at it

The arguments are empty by default on purpose, and so is the opening
input beyond what delivery needs.  An agent started here runs in a
terminal you are watching, so which permissions it should have, and which
mode it should work in, are yours to decide rather than this package's to
assume.  Codex's two settings are the exception and neither is a choice of
that kind: both are what make a typed brief arrive at all.  See
`envoy-tmux-opening-input' for how to ask codex for plan mode."
  :type '(alist :key-type symbol :value-type plist)
  :group 'envoy)


(defcustom envoy-tmux-window-name-width 40
  "How much of a heading to use as the tmux window name.
Every window shares the status line, so a long name costs every other
window its own."
  :type 'integer
  :group 'envoy)

(defun envoy-tmux--provider (&optional provider)
  "Return the plist for PROVIDER, or for `envoy-tmux-provider'.

An entry with no :program is refused here rather than further down.
`setf' on an `alist-get' whose key is absent creates the entry, so an init
file that configures an agent this version of envoy has never heard of
builds a plist holding only what it set -- and the first thing to notice
used to be `shell-quote-argument' rejecting nil, which names neither the
agent nor the cause.

That creation also puts the new entry at the FRONT, which is why a second
entry under the same name is worth its own message.  `assq' stops at the
first one, so a real definition sitting behind a manufactured one is never
reached, and a check for the name alone is satisfied by the copy that
cannot work."
  (let* ((key (or provider envoy-tmux-provider))
         (entry (assq key envoy-tmux-providers))
         (copies (seq-count (lambda (e) (eq (car-safe e) key))
                            envoy-tmux-providers)))
    (unless entry
      (user-error "Envoy: no tmux provider named %s" key))
    (unless (plist-get (cdr entry) :program)
      (user-error "Envoy: the %s provider names no :program%s" key
                  (cond ((> copies 1)
                         (format ", and %d entries share that name, so the \
one being read is shadowing the one that works" copies))
                        ((cdr entry)
                         ", so something has configured it without defining it")
                        (t ""))))
    (cdr entry)))

(defun envoy-tmux--provider-program (&optional provider)
  "Return PROVIDER's executable path, or signal a user error."
  (let* ((plist (envoy-tmux--provider provider))
         (program (plist-get plist :program)))
    (unless (and (stringp program) (not (string-empty-p program)))
      (user-error "Envoy: tmux provider %s has no executable"
                  (envoy-tmux-provider-name provider)))
    (or (executable-find program)
        (user-error "Envoy: %s needs `%s', which is not on `exec-path'"
                    (envoy-tmux-provider-name provider) program))))


(defun envoy-tmux-provider-name (&optional provider)
  "Return the human-readable name of PROVIDER, or of `envoy-tmux-provider'."
  (or (plist-get (envoy-tmux--provider provider) :name)
      (symbol-name (or provider envoy-tmux-provider))))

(defun envoy-tmux--tmux ()
  "Return the absolute path to tmux, or signal that it is not installed."
  (or (executable-find envoy-tmux-program)
      (user-error "Envoy: `%s' is not on `exec-path'" envoy-tmux-program)))

(defun envoy-tmux--call (&rest args)
  "Run tmux with ARGS and return (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((status (apply #'call-process (envoy-tmux--tmux) nil t nil args)))
      (cons status (string-trim (buffer-string))))))

(defun envoy-tmux-session-live-p (session)
  "Return non-nil when a tmux session named exactly SESSION is running."
  (and session
       (zerop (car (envoy-tmux--call "has-session" "-t"
                                     (concat "=" session))))))

(defun envoy-tmux-sessions ()
  "Return the names of the tmux sessions running now, in tmux's own order.
The list is fetched for every dispatch rather than cached.  A session started
or killed in a terminal between two dispatches is the ordinary case, so a
cached list would ask about a session that is gone or hide one that is there."
  (let ((answer (envoy-tmux--call "list-sessions" "-F" "#{session_name}")))
    (when (zerop (car answer))
      (split-string (cdr answer) "\n" t))))

(defvar envoy-tmux--session-history nil
  "Minibuffer history for `envoy-tmux-read-session'.")

(defun envoy-tmux--session-table (sessions)
  "Return a completion table for SESSIONS in tmux's own order.
The picker shows what tmux shows, so the name seen in the status line is where
it is found in the list.  A name absent from SESSIONS remains acceptable
because that is how a new session is started, and `envoy-tmux-window' already
runs `new-session' for a name that is not live."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata
          (category . envoy-tmux-session)
          (display-sort-function . identity)
          (cycle-sort-function . identity))
      (complete-with-action action sessions string predicate))))

(defun envoy-tmux-read-session (&optional provider)
  "Ask which tmux session PROVIDER should be started in, and return the name.
The live list is read for every invocation rather than cached, because a
session started or killed in a terminal between two dispatches is ordinary.
Names not in that list are accepted so `envoy-tmux-window' can start one.
There is no default and no initial input."
  (let* ((sessions (envoy-tmux-sessions))
         (answer (completing-read
                  (format "tmux session for %s (or a new name): "
                          (envoy-tmux-provider-name provider))
                  (envoy-tmux--session-table sessions)
                  nil nil nil 'envoy-tmux--session-history nil))
         (session (substring-no-properties
                   (string-trim (or answer "")))))
    (cond
     ((string-empty-p session)
      (user-error "Envoy: no tmux session was selected"))
     ((string-match-p "[.:]" session)
      (user-error
       "Envoy: tmux turns `.' and `:' into `_', so session \"%s\" would be unreachable under the name asked for"
       session))
     (t session))))

(defun envoy-tmux-window-name (name)
  "Return NAME as a tmux window name.
Shortened to `envoy-tmux-window-name-width', with whitespace and
anything tmux reads as syntax replaced by dashes."
  (let ((short (truncate-string-to-width
                (string-trim name) envoy-tmux-window-name-width nil nil "…")))
    (replace-regexp-in-string
     "[^-a-zA-Z0-9_./…]" "-"
     (replace-regexp-in-string "[ \t\n]+" "-" short))))
(defun envoy-tmux-window (session name)
  "Add a window named NAME to tmux session SESSION and return its target.
The session is started if it is not running.  The target comes back as
session:index rather than session:name, which stays unambiguous when two
windows are named the same — and two tasks with the same heading are
exactly what a datetree collects."
  (let* ((window-name (envoy-tmux-window-name name))
         (created (not (envoy-tmux-session-live-p session)))
         (window-args (list "new-window" "-d" "-P"
                            "-F" "#{session_name}:#{window_index}"
                            "-t" (concat "=" session)
                            "-n" window-name))
         (answer
          (if created
              (envoy-tmux--call
               "new-session" "-d" "-P"
               "-F" "#{session_name}:#{window_index}"
               "-s" session
               "-n" window-name)
            (apply #'envoy-tmux--call window-args))))
    (when (and created
               (not (zerop (car answer)))
               (envoy-tmux-session-live-p session))
      (setq answer (apply #'envoy-tmux--call window-args)))

    (unless (zerop (car answer))
      (user-error "Envoy: tmux would not open a window in %s: %s"
                  session (cdr answer)))
    (cdr answer)))

(defun envoy-tmux--write-prompt (prompt)
  "Write PROMPT to a file for an agent to read, and return the file.
Readable by its owner alone: a brief carries whatever the heading and its
ancestors carry, which on a shared machine is not everybody's business.
The command sent to a file-reading provider removes the file after the
provider exits."
  (let ((file (make-temp-file "envoy-prompt-")))
    (with-temp-file file (insert prompt))
    (set-file-modes file #o600)
    file))

(defcustom envoy-tmux-omp-query-timeout 5
  "Seconds to wait for an OMP model query before refusing the dispatch."
  :type 'number
  :group 'envoy)
(defcustom envoy-tmux-omp-drive-program "omp-drive"
  "The OMP Drive wrapper executable, looked up on the variable `exec-path'."
  :type 'string
  :group 'envoy)

(defun envoy-tmux--omp-drive-program ()
  "Return OMP's Drive wrapper path, or signal an actionable user error."
  (or (executable-find envoy-tmux-omp-drive-program)
      (user-error "Envoy: OMP Drive needs `%s', which is not on `exec-path'"
                  envoy-tmux-omp-drive-program)))


(defun envoy-tmux--omp-program ()
  "Return OMP's executable path, or signal an actionable user error."
  (let* ((plist (envoy-tmux--provider 'omp))
         (program (plist-get plist :program)))
    (or (and (stringp program) (executable-find program))
        (user-error "Envoy: OMP needs `%s', which is not on `exec-path'"
                    program))))

(defun envoy-tmux--omp-process (program args)
  "Run OMP PROGRAM with ARGS and return its status and output.
The result is a plist with :status, :stdout, :stderr and optional :timeout."
  (let ((stdout (generate-new-buffer " *envoy-omp-stdout*"))
        (stderr (generate-new-buffer " *envoy-omp-stderr*"))
        process result)
    (unwind-protect
        (progn
          (condition-case err
              (setq process
                    (make-process
                     :name (generate-new-buffer-name "envoy-omp-query")
                     :buffer stdout
                     :stderr stderr
                     :command (cons program args)
                     :connection-type 'pipe
                     :sentinel #'ignore
                     :noquery t))
            (error
             (user-error "Envoy: could not run OMP `%s': %s"
                         program (error-message-string err))))
          (ignore-errors (process-send-eof process))
          (let ((deadline (+ (float-time) envoy-tmux-omp-query-timeout)))
            (while (and (process-live-p process)
                        (< (float-time) deadline))
              (accept-process-output
               process (max 0.01 (min 0.1 (- deadline (float-time)))))))
          (if (process-live-p process)
              (progn
                (delete-process process)
                (setq result (list :timeout t)))
            (setq result
                  (list :status (process-exit-status process)
                        :stdout (with-current-buffer stdout
                                  (buffer-string))
                        :stderr (with-current-buffer stderr
                                  (buffer-string)))))
          result)
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer stdout)
      (kill-buffer stderr))))

(defun envoy-tmux--omp-json (program args)
  "Run OMP PROGRAM with ARGS and parse its JSON output.
Signal a user error for a timeout, a failed command, empty output or invalid
JSON rather than handing an empty model list to the completion UI."
  (let* ((result (envoy-tmux--omp-process program args))
         (status (plist-get result :status))
         (stdout (or (plist-get result :stdout) ""))
         (stderr (string-trim (or (plist-get result :stderr) "")))
         (command (mapconcat #'identity args " "))
         (parsed (and (zerop (or status 1))
                      (not (string-empty-p (string-trim stdout)))
                      (envoy--parse-json stdout))))
    (cond
     ((plist-get result :timeout)
      (user-error "Envoy: OMP `%s' timed out after %s seconds"
                  command envoy-tmux-omp-query-timeout))
     ((not (and status (zerop status)))
      (user-error "Envoy: OMP `%s' failed%s"
                  command
                  (if (string-empty-p stderr) "."
                    (format ": %s" stderr))))
     ((string-empty-p (string-trim stdout))
      (user-error "Envoy: OMP `%s' returned no JSON output" command))
     ((null parsed)
      (user-error "Envoy: OMP `%s' returned unreadable JSON" command))
     ((not (and (listp parsed)
                (seq-every-p (lambda (entry)
                               (and (consp entry) (symbolp (car entry))))
                             parsed)))
      (user-error "Envoy: OMP `%s' returned a non-object JSON value" command))
     (t parsed))))

(defun envoy-tmux--omp-normalize-models (values)
  "Return non-empty string selectors from VALUES without duplicates.
Signal when a supplied selector is not a non-empty string."
  (unless (or (null values) (stringp values) (listp values))
    (user-error "Envoy: OMP model selectors must be strings"))
  (let ((items (if (stringp values) (list values) values)))
    (when (seq-some (lambda (value)
                      (or (not (stringp value))
                          (string-empty-p value)))
                    items)
      (user-error "Envoy: OMP model selectors contain an invalid value"))
    (seq-uniq items)))

(defun envoy-tmux--omp-model-selector (model)
  "Return MODEL's full selector, or nil when it has none."
  (cond
   ((and (stringp model) (not (string-empty-p model))) model)
   ((listp model)
    (or (let ((selector (cdr (assq 'selector model))))
          (and (stringp selector)
               (not (string-empty-p selector))
               selector))
        (let ((provider (cdr (assq 'provider model)))
              (id (cdr (assq 'id model))))
          (and (stringp provider) (stringp id)
               (not (string-empty-p provider))
               (not (string-empty-p id))
               (format "%s/%s" provider id)))))))

(defun envoy-tmux--omp-catalog-models (json)
  "Return full non-wildcard selectors from an OMP models JSON object."
  (let ((models (cdr (assq 'models json))))
    (seq-remove
     #'envoy-tmux--omp-wildcard-p
     (envoy-tmux--omp-normalize-models
      (and (listp models)
           (delq nil (mapcar #'envoy-tmux--omp-model-selector models)))))))

(defun envoy-tmux--omp-wildcard-p (selector)
  "Return non-nil when SELECTOR is a wildcard pattern."
  (and (stringp selector) (string-match-p "[*?]" selector)))

(defun envoy-tmux--omp-expand-models (program values)
  "Expand wildcard VALUES against OMP's model catalog using PROGRAM."
  (let ((patterns (seq-filter #'envoy-tmux--omp-wildcard-p values))
        (exact (seq-remove #'envoy-tmux--omp-wildcard-p values)))
    (if (null patterns)
        values
      (let* ((catalog-json (envoy-tmux--omp-json program '("models" "--json")))
             (catalog (envoy-tmux--omp-catalog-models catalog-json))
             (expanded
              (seq-filter
               (lambda (selector)
                 (seq-some
                  (lambda (pattern)
                    (string-match-p (wildcard-to-regexp pattern) selector))
                  patterns))
               catalog)))
        (unless expanded
          (user-error "Envoy: OMP enabledModels patterns matched no model selectors"))
        (seq-uniq (append exact expanded))))))

(defun envoy-tmux--omp-discover-models (program)
  "Discover OMP selectors using PROGRAM's enabledModels or model catalog."
  (let (enabled-json enabled enabled-error)
    (condition-case err
        (setq enabled-json
              (envoy-tmux--omp-json
               program '("config" "get" "enabledModels" "--json")))
      (user-error
       (setq enabled-error (cadr err))))
    (when enabled-json
      (setq enabled
            (envoy-tmux--omp-normalize-models
             (cdr (assq 'value enabled-json)))))
    (if enabled
        (envoy-tmux--omp-expand-models program enabled)
      (let (catalog catalog-error)
        (condition-case err
            (setq catalog
                  (envoy-tmux--omp-catalog-models
                   (envoy-tmux--omp-json program '("models" "--json"))))
          (user-error
           (setq catalog-error (cadr err))))
        (if catalog
            catalog
          (user-error
           "Envoy: OMP model discovery failed%s%s"
           (if enabled-error
               (format "; enabledModels query: %s" enabled-error)
             "")
           (if catalog-error
               (format "; model catalog query: %s" catalog-error)
             "; OMP returned no model selectors")))))))

(defun envoy-tmux--omp-models ()
  "Return OMP model selectors for completion.
An explicit non-empty provider :models list wins.  Otherwise consult
`enabledModels' and fall back to the native model catalog."
  (let* ((plist (envoy-tmux--provider 'omp))
         (program (envoy-tmux--omp-program))
         (configured (envoy-tmux--omp-normalize-models
                      (plist-get plist :models))))
    (if configured
        (envoy-tmux--omp-expand-models program configured)
      (envoy-tmux--omp-discover-models program))))

(defun envoy-tmux-command (prompt-file &optional provider model setup plan-into
                                             mode)
  "Return the shell command that will start PROVIDER on PROMPT-FILE.
MODEL is passed through the provider's :model-arg when given.  SETUP is a
shell command to run first, defaulting to the provider's own :setup, and
is what a wrapper that selects a provider or a profile goes in.  For OMP,
the modes are Single model when MODEL is set, Plan into when PLAN-INTO is
set, and Drive when MODE is `drive'.

The result is one shell command line, because it is typed into a shell in
a tmux window rather than executed here.  Everything variable in it is
quoted for that shell; :prompt-arg is not, since it is the provider's own
template and may need shell syntax of its own to deliver the prompt."
  (let* ((plist (envoy-tmux--provider provider))
         (omp (eq (or provider envoy-tmux-provider) 'omp))
         (drive (and omp (eq mode 'drive)))
         (program (if drive
                      (envoy-tmux--omp-drive-program)
                    (envoy-tmux--provider-program provider)))
         (setup (if (null setup) (plist-get plist :setup) setup))
         (prompt-arg (plist-get plist :prompt-arg))
         (explicit-model (if omp
                             (and (stringp model) (not (string-empty-p model)))
                           model))
         (explicit-plan (and (stringp plan-into)
                             (not (string-empty-p plan-into))))
         (omp-plan (and omp explicit-plan (not drive)))
         (args (plist-get plist :args)))
    (when (and mode (not drive))
      (user-error "Envoy: Drive mode is only supported by the OMP provider"))
    (when (and omp
               (or (and model (not explicit-model))
                   (and plan-into (not explicit-plan))))
      (user-error "Envoy: OMP model selections must not be empty"))
    (when (and plan-into (not omp))
      (user-error "Envoy: plan-into is only supported by the OMP provider"))
    (when (and drive plan-into)
      (user-error "Envoy: Drive mode cannot select an execution model"))
    (when (and omp-plan (not explicit-model))
      (user-error "Envoy: OMP plan mode needs a planner model"))
    (when (and omp (or explicit-model drive))
      (setq args
            (let ((filtered nil)
                  (skip-next nil))
              (dolist (arg args (nreverse filtered))
                (cond
                 (skip-next (setq skip-next nil))
                 ((or (string-match-p "\\`--model=" arg)
                      (string-match-p "\\`--plan-yolo-into=" arg)
                      (string-match-p "\\`--plan-yolo=" arg)) nil)
                 ((member arg '("--plan-yolo")) nil)
                 ((member arg '("--model" "--plan-yolo-into"))
                  (setq skip-next t))
                 (t (push arg filtered)))))))
    (concat (when (and setup (not (string-empty-p setup)))
              (concat setup " && "))
            (when (and drive explicit-model)
              (concat "OMP_DRIVE_MODEL=" (shell-quote-argument model) " "))
            (shell-quote-argument program)
            (mapconcat (lambda (arg) (concat " " (shell-quote-argument arg)))
                       (append args
                               (cond
                                (omp-plan
                                 (list "--model" model
                                       "--plan-yolo"
                                       "--plan-yolo-into" plan-into))
                                ((and explicit-model (plist-get plist :model-arg)
                                      (not drive))
                                 (list (plist-get plist :model-arg) model))))
                       "")
            (when prompt-arg
              (concat " " (format prompt-arg
                                  (shell-quote-argument prompt-file)))))))



(defconst envoy-tmux--type-chunk 4096
  "How much of a typed prompt to hand tmux at a time.
Under the terminal's own input queue, which is the limit that decides
this.  See `envoy-tmux-type'.")

(defcustom envoy-tmux-type-pause 0.15
  "Seconds to wait between the pieces of a typed prompt.
Long enough for the agent to empty the terminal's input queue before the
next piece is written into it.  See `envoy-tmux-type' for what happens
when it is not."
  :type 'number
  :group 'envoy)

(defcustom envoy-tmux-start-timeout 5
  "Seconds to wait for an agent to start before typing a prompt at it.
Only agents that read their prompt from the terminal wait at all.  When
it runs out, typed input is refused rather than sent to the shell."
  :type 'number
  :group 'envoy)
(defcustom envoy-tmux-shells
  '("sh" "bash" "zsh" "fish" "dash" "ksh" "tcsh" "csh"
    "nu" "nushell" "pwsh" "powershell" "cmd" "cmd.exe"
    "elvish" "xonsh" "ion" "sleep")
  "Commands that mean a pane is still at its shell prompt."
  :type '(repeat string)
  :group 'envoy)



(defun envoy-tmux--wait-for-agent (target &optional program)
  "Wait until tmux TARGET is running PROGRAM.
When PROGRAM is nil, wait until it is running a non-shell command.
Returns non-nil when it is.  Each status probe is bounded by the remaining
startup deadline."
  (let ((deadline (+ (float-time) envoy-tmux-start-timeout))
        (expected (and program (file-name-nondirectory program)))
        (started nil))
    (while (and (not started) (< (float-time) deadline))
      (let* ((remaining (- deadline (float-time)))
             (answer (and (> remaining 0)
                          (condition-case nil
                              (with-timeout (remaining nil)
                                (envoy-tmux--call
                                 "display-message" "-p" "-t" target
                                 "#{pane_current_command}"))
                            (timeout nil)))))
        (let ((command (and answer (cdr answer))))
          (if (and answer
                   (zerop (car answer))
                   command
                   (not (string-empty-p command))
                   (if expected
                       (and envoy-tmux-shells
                            (or (equal (file-name-nondirectory command) expected)
                                (and (file-name-directory program)
                                     (not (member (file-name-nondirectory command)
                                                  envoy-tmux-shells)))))
                     (and envoy-tmux-shells
                          (not (member (file-name-nondirectory command)
                                       envoy-tmux-shells)))))
              (setq started t)
            (let ((pause (min 0.05 (max 0 remaining))))
              (when (> pause 0)
                (sleep-for pause)))))))
    started))




(defun envoy-tmux-type (target text &optional program wait)
  "Type TEXT into tmux TARGET as literal input, then submit it.
For an agent that reads its prompt from the terminal rather than from a
file or an argument.  Sent literally, so tmux reads none of it as key
names.  The prompt is split on encoded byte size without splitting a
character.  PROGRAM is the executable expected in the pane.  When WAIT
is non-nil, return only after every piece and the final Enter succeed."
  (unless (envoy-tmux--wait-for-agent target program)
    (user-error "Envoy: agent did not start in tmux target %s" target))

  (let ((start 0)
        (end (length text))
        (done nil)
        (cancelled nil)
        timer
        failure
        send-next
        (submit
         (lambda ()
           (let ((answer (envoy-tmux--call "send-keys" "-t" target "Enter")))
             (unless (and answer (zerop (car answer)))
               (user-error "Envoy: tmux could not submit the prompt in %s: %s"
                           target (cdr answer)))))))
    (setq send-next
          (lambda (asynchronous)
            (when asynchronous
              (setq timer nil))
            (unless cancelled
              (condition-case err
                  (if (< start end)
                      (let ((stop start)
                            (bytes 0))
                        (while (and (< stop end)
                                    (let ((next-bytes
                                           (+ bytes
                                              (string-bytes
                                               (substring text stop (1+ stop))))))
                                      (when (<= next-bytes envoy-tmux--type-chunk)
                                        (setq bytes next-bytes
                                              stop (1+ stop))
                                        t))))
                        (let ((answer
                               (envoy-tmux--call "send-keys" "-t" target "-l"
                                                 (substring text start stop))))
                          (unless (and answer (zerop (car answer)))
                            (user-error
                             "Envoy: tmux could not type the prompt in %s: %s"
                             target (cdr answer))))
                        (setq start stop)
                        (if (< start end)
                            (unless wait
                              (setq timer
                                    (run-at-time envoy-tmux-type-pause nil
                                                 send-next t)))
                          (funcall submit)
                          (setq done t)))
                    (funcall submit)
                    (setq done t))
                (error
                 (if asynchronous
                     (if wait
                         (setq failure err
                               done t)
                       (message "%s" (error-message-string err)))
                   (signal (car err) (cdr err))))))))
    (unwind-protect
        (if wait
            (progn
              (while (not done)
                (funcall send-next nil)
                (when (not done)
                  (sleep-for envoy-tmux-type-pause)))
              (when failure
                (signal (car failure) (cdr failure))))
          (funcall send-next nil))
      (unless (and (not wait) timer)
        (setq cancelled t)
        (when timer
          (cancel-timer timer))))))


(defun envoy-tmux--prompt-file-cleanup (target file)
  "Return the command to remove FILE after the provider exits in TARGET."
  (let* ((answer (envoy-tmux--call "display-message" "-p" "-t" target
                                  "#{pane_current_command}"))
         (shell (and answer
                     (zerop (car answer))
                     (cdr answer))))
    (if (and shell
             (member (file-name-nondirectory shell) '("cmd" "cmd.exe")))
        (concat " & del /q \"" file "\"")
      (concat "; rm -f -- " (shell-quote-argument file)))))

(defcustom envoy-tmux-draw-timeout 20
  "Seconds to wait for an agent's interface to appear before typing at it.
Only agents with :opening-input wait, and only for as long as the pane is
still changing.  See `envoy-tmux--wait-for-interface'."
  :type 'number
  :group 'envoy)

(defun envoy-tmux--wait-for-interface (target)
  "Wait until what tmux TARGET is drawing stops changing, and return non-nil.
For sending a keystroke rather than text.  `envoy-tmux--wait-for-agent'
waits for the pane to stop running a shell, which is not the same thing
and is not enough here: measured, the pane stops being a shell within a
tenth of a second, at the wrapper, and the interface it is going to draw
appears half a second later.  A brief typed into that gap is only buffered
and arrives late, which costs nothing — but a keystroke sent into it is
taken by whatever is there when it lands.  Measured: an Enter sent at a
tenth of a second was lost, and the keys after it left the pane back at
its shell with the agent gone.

So the pane is captured until two captures running agree, which is the
agent having drawn and stopped.  Measured over repeated runs at half a
second in a directory it has to ask about and a second in one it does not,
every time.  A pane that never settles gives up after
`envoy-tmux-draw-timeout' rather than waiting on it."
  (let ((deadline (+ (float-time) envoy-tmux-draw-timeout))
        (previous nil)
        (settled nil))
    (while (and (not settled) (< (float-time) deadline))
      (let ((shot (cdr (envoy-tmux--call "capture-pane" "-p" "-t" target))))
        (if (and previous (equal shot previous) (not (string-empty-p shot)))
            (setq settled t)
          (setq previous shot)
          (sleep-for envoy-tmux-type-pause))))
    settled))

(defun envoy-tmux-opening-input (target provider)
  "Send PROVIDER's :opening-input at tmux TARGET, ahead of the brief.
For a mode an agent will only take from inside its own interface.  The
interface is waited for by `envoy-tmux--wait-for-interface', which is not the
same wait a typed brief takes and is not optional.  A keystroke sent before
there is something to read it is taken by whatever is there instead, and the
dispatch can end with the agent gone.  Each line is then typed and submitted
in turn.  The typed sender waits for the configured provider before each
line, sends all chunks and its final Enter, and propagates a deferred send
failure.  A pause follows each submitted line so a slash command has time to
settle before the next command.

The pause matters because a line beginning with a slash opens a live
completion popup rather than text.  The next typed input must not arrive while
the popup is resolving, or `/plan' may remain in the composer.  A seventh of
a second is enough, and is the same pause a typed brief already takes between
its pieces.  At a composer with nothing in it the initial Enter does nothing,
so the default Codex launch costs the agent no extra work.

Codex ships with that Enter and nothing else, because a mode is yours to
choose.  To have it plan rather than act, add the command it offers for
that:

  (setf (plist-get (alist-get \\'codex envoy-tmux-providers) :opening-input)
        \\'(\"\" \"/plan\"))

Which is the only route codex offers.  The mode is a parameter of a turn,
chosen from inside the interface: there is no option for it on the command
line, no key for it in the configuration file, and codex exec cannot carry
it at all."
  (let ((lines (plist-get (envoy-tmux--provider provider) :opening-input)))
    (when lines
      (envoy-tmux--wait-for-agent target)
      (unless (envoy-tmux--wait-for-interface target)
        (user-error "Envoy: interface did not settle in tmux target %s" target))
      (dolist (line lines)
        (envoy-tmux-type target line
                          (plist-get (envoy-tmux--provider provider) :program)
                          t)
        (sleep-for envoy-tmux-type-pause)))))

(defun envoy-tmux-send (target prompt &optional provider model setup plan-into
                                      mode on-start)
  "Start PROVIDER in tmux TARGET and give it PROMPT.
MODEL, SETUP, PLAN-INTO and MODE are as in `envoy-tmux-command'.  Return the
file the brief was written to when PROVIDER reads one, or nil when it is
typed.  OMP's modes are Single model, Plan into and Drive.  ON-START is
called immediately after the launch command is submitted, before Envoy waits
for or types anything else.

The command line is sent literally and submitted with an Enter of its own,
so the quotes and dollar signs a provider's :prompt-arg may hold reach the
shell instead of being read as key names.  Once that Enter succeeds, the
provider may still need its prompt file even when a later send step fails."
  (let* ((plist (envoy-tmux--provider provider))
         (prompt-arg (plist-get plist :prompt-arg))
         (file (when prompt-arg (envoy-tmux--write-prompt prompt)))
         (launched nil))
    (unwind-protect
        (progn
          (let ((command (envoy-tmux-command file provider model setup
                                             plan-into mode)))
            (when prompt-arg
              (setq command
                    (concat command
                            (envoy-tmux--prompt-file-cleanup target file))))
            (let ((answer (envoy-tmux--call "send-keys" "-t" target "-l"
                                            command)))
              (unless (and answer (zerop (car answer)))
                (user-error "Envoy: tmux could not send the command in %s: %s"
                            target (cdr answer))))
            (let ((answer (envoy-tmux--call "send-keys" "-t" target "Enter")))
              (unless (and answer (zerop (car answer)))
                (user-error "Envoy: tmux could not submit the command in %s: %s"
                            target (cdr answer))))
            (setq launched t)
            (when on-start
              (funcall on-start)))
          (unless prompt-arg
            (envoy-tmux-opening-input target provider)
            (envoy-tmux-type target prompt (plist-get plist :program) t))
          file)
      (unless launched
        (when (and file (file-exists-p file))
          (delete-file file))))))

(defun envoy-tmux--read-variable (provider)
  "Ask for the prefix-controlled selection for PROVIDER.
Return a plist with :model, :setup, :plan-into and :mode.  OMP offers
Single model, Plan into and Drive; other providers retain their one
model-or-setup choice."
  (let* ((plist (envoy-tmux--provider provider))
         (variable (plist-get plist :variable))
         (choices (plist-get plist (if (eq variable 'model) :models :setups))))
    (if (eq provider 'omp)
        (let ((models (envoy-tmux--omp-models)))
          (unless models
            (user-error "Envoy: OMP supplied no model selectors"))
          (let ((mode (completing-read "OMP launch mode: "
                                       '("Single model" "Plan into" "Drive")
                                       nil t))
                (read-model
                 (lambda (prompt &optional available-models)
                   (let* ((completion-styles '(substring basic))
                          (model (completing-read
                                  prompt (or available-models models)
                                  nil nil)))
                     (if (string-empty-p model)
                         (user-error "Envoy: OMP model selection was empty")
                       model)))))
            (cond
             ((equal mode "Single model")
              (list :model (funcall read-model "Model for OMP: ")
                    :setup nil :plan-into nil))
             ((equal mode "Plan into")
              (let ((planner (funcall read-model "Planner model for OMP: "))
                    (executor (funcall read-model "Execution model for OMP: ")))
                (list :model planner :setup nil :plan-into executor)))
             ((equal mode "Drive")
              (let ((driver (funcall read-model "Driver model for OMP: "
                                      (cons "Default driver" models))))
                (list :model (unless (equal driver "Default driver") driver)
                      :setup nil :plan-into nil :mode 'drive)))
             (t (user-error "Envoy: unknown OMP launch mode %s" mode)))))
      (if (eq variable 'model)
          (let ((model (completing-read
                        (format "Model for %s: "
                                (envoy-tmux-provider-name provider))
                        choices nil nil)))
            (if (string-empty-p model)
                (user-error "Envoy: %s model selection was empty"
                            (envoy-tmux-provider-name provider))
              (list :model model :setup nil :plan-into nil)))
        (list :model nil
              :setup (completing-read
                      (format "Command before %s: "
                              (envoy-tmux-provider-name provider))
                      choices nil nil)
              :plan-into nil)))))

(defun envoy-tmux--attachment-state (directory)
  "Return the current heading and attachment state for DIRECTORY."
  (let ((files (and directory
                    (file-directory-p directory)
                    (condition-case nil
                        (directory-files directory nil
                                         "\\`[^.]\\|\\`\\.[^.]" t)
                      (file-error :unknown)))))
    (if (eq files :unknown)
        :unknown
      (condition-case nil
          (let ((attributes (and buffer-file-name
                                 (file-attributes buffer-file-name))))
            (if (and buffer-file-name (null attributes))
                :unknown
              (list (org-get-heading t t t t)
                    (mapcar (lambda (tag) (substring-no-properties tag))
                            (org-get-tags nil t))
                    (buffer-chars-modified-tick)
                    (and attributes
                         (file-attribute-modification-time attributes))
                    files)))
        (file-error :unknown)))))
(defun envoy-tmux--claim-owned-p (claim)
  "Return non-nil when CLAIM's token still owns its claim file."
  (let ((file (plist-get claim :file))
        (token (plist-get claim :token)))
    (and file token
         (file-exists-p file)
         (equal token (envoy-org--spool-claim-token file)))))

(defun envoy-tmux-org-heading-to (provider &optional model setup plan-into
                                             session mode select)
  "Hand the org heading at point to PROVIDER in a tmux window.
MODEL, SETUP, PLAN-INTO and MODE are as in `envoy-tmux-command'.  SESSION is the
name to use, or nil to ask for one.  When SELECT is non-nil, ask for the
provider's model, setup and OMP mode after claiming the heading.

The heading is claimed by its Org ID before provider discovery, selection,
session selection or window creation.  An active claim or pending report
marker stops a second dispatch.  Once the launch command is submitted, the
claim, state and attachment preparation stay in place until report
collection, even if a later send step or keyword update fails.

The brief is the same one `envoy-org-heading' sends: the whole subtree,
every ancestor heading that is not datetree scaffolding, and the attachment
directories of the heading and of those ancestors.  What differs is the
delivery.  The agent runs in a terminal you can attach to and answer."
  (unless (derived-mode-p 'org-mode)
    (user-error "Envoy: not an org buffer"))
  (unless (buffer-file-name)
    (user-error "Envoy: this org buffer is not visiting a file"))
  (when (buffer-modified-p)
    (if (y-or-n-p "Save this buffer first? ")
        (save-buffer)
      (user-error "Envoy: the buffer has unsaved changes")))
  (org-back-to-heading t)
  (let* ((title (org-get-heading t t t t))
         (subtree (envoy-org--subtree-text))
         (attachment-root
          (expand-file-name org-attach-id-dir
                            (file-name-directory
                             (or buffer-file-name default-directory))))
         (previous-attachment-root (file-directory-p attachment-root))
         (previous-attach-dir (ignore-errors (org-attach-dir nil t)))
         attach-tag
         attach-tag-present-before-setup
         target
         attach-dir
         attach-tag-created
         attachment-state
         claim-key
         claim
         launched)
    (let* ((identity (envoy-org--heading-identity))
           (id (nth 0 identity)))
      (setq claim (envoy-org--spool-claim id title)
            claim-key (plist-get claim :key))
      (unwind-protect
          (progn
            (when select
              (let ((choice (envoy-tmux--read-variable provider)))
                (setq model (plist-get choice :model)
                      setup (plist-get choice :setup)
                      plan-into (plist-get choice :plan-into)
                      mode (plist-get choice :mode))))
            (envoy-tmux--provider-program provider)
            (when (eq provider 'omp)
              (envoy-tmux--omp-program)
              (when (eq mode 'drive)
                (envoy-tmux--omp-drive-program)))
            (setq session (or session (envoy-tmux-read-session provider)))
            (setq target (envoy-tmux-window session title))
            (when (buffer-modified-p)
              (save-buffer))
            (setq attach-tag
                  (and org-attach-auto-tag
                       (if (stringp org-attach-auto-tag)
                           org-attach-auto-tag
                         "ATTACH")))
            (setq attach-tag-present-before-setup
                  (and attach-tag
                       (member attach-tag (org-get-tags nil t))))
            (setq attach-dir (envoy-org--attach-directory))
            (setq attach-tag-created
                  (and attach-tag
                       (not attach-tag-present-before-setup)
                       (member attach-tag (org-get-tags nil t))))
            (setq attachment-state
                  (and attach-dir
                       (envoy-tmux--attachment-state attach-dir)))
            (let* ((task-files (envoy-org--attachment-files attach-dir))
                   (spool-key (envoy-org--spool-open title task-files))
                   (prompt (envoy-org-build-prompt title subtree attach-dir
                                                   task-files spool-key)))
              (envoy-tmux-send
               target prompt provider model setup plan-into mode
               (lambda () (setq launched t)))
              (setq launched t)
              (when (envoy-tmux--claim-owned-p claim)
                (envoy-org--set-keyword envoy-org-active-keyword)))
            (message "Envoy: %s has \"%s\" in tmux %s"
                     (envoy-tmux-provider-name provider) title target)
            target)
        (unless launched
          (when (envoy-tmux--claim-owned-p claim)
            (when claim-key
              (ignore-errors
                (let ((state (envoy-org--spool-file
                              claim-key envoy-org--spool-state-suffix)))
                  (when (file-exists-p state)
                    (delete-file state)))))
            (let ((attachment-state-unchanged
                   (and (consp attachment-state)
                        (condition-case nil
                            (let ((current-state
                                   (envoy-tmux--attachment-state attach-dir)))
                              (and (consp current-state)
                                   (equal attachment-state current-state)))
                          (file-error nil)))))
              (when (and attachment-state-unchanged
                         attach-dir
                         (not previous-attach-dir)
                         (file-directory-p attach-dir))
                (let ((directory (file-name-as-directory attach-dir))
                      (root (file-name-as-directory attachment-root)))
                  (while (and (file-directory-p directory)
                              (string-prefix-p root directory))
                    (if (and (equal directory root)
                             previous-attachment-root)
                        (setq directory nil)
                      (condition-case nil
                          (when (null (directory-files directory nil
                                                       "\\`[^.]\\|\\`\\.[^.]" t))
                            (ignore-errors (delete-directory directory)))
                        (file-error nil))
                      (setq directory
                            (file-name-as-directory
                             (file-name-directory
                              (directory-file-name directory))))))))
              (when (and attachment-state-unchanged
                         attach-tag-created
                         (member attach-tag (org-get-tags nil t)))
                (ignore-errors
                  (org-toggle-tag attach-tag 'off)
                  (when (and (buffer-file-name) (buffer-modified-p))
                    (save-buffer)))))
            (envoy-org--spool-release-claim claim)))))))
;;;###autoload
(defun envoy-tmux-org-heading (&optional arg)
  "Hand the org heading at point to `envoy-tmux-provider' in a tmux window.
With a prefix argument ARG, ask for that provider's selection after claiming
the heading."
  (interactive "P")
  (envoy-tmux-org-heading-to envoy-tmux-provider
                             nil nil nil nil nil arg))

(defun envoy-tmux--dispatch (provider arg)
  "Hand the heading at point to PROVIDER, selecting it after claiming when ARG."
  (envoy-tmux-org-heading-to provider nil nil nil nil nil arg))

;;;###autoload
(defun envoy-tmux-org-heading-claude (&optional arg)
  "Hand the org heading at point to Claude Code in a tmux window.
With a prefix argument ARG, ask first for what its :variable names."
  (interactive "P")
  (envoy-tmux--dispatch 'claude arg))

;;;###autoload
(defun envoy-tmux-org-heading-reasonix (&optional arg)
  "Hand the org heading at point to reasonix in a tmux window.
With a prefix argument ARG, ask first for what its :variable names."
  (interactive "P")
  (envoy-tmux--dispatch 'reasonix arg))

;;;###autoload
(defun envoy-tmux-org-heading-pi (&optional arg)
  "Hand the org heading at point to pi in a tmux window.
With a prefix argument ARG, ask first for what its :variable names."
  (interactive "P")
  (envoy-tmux--dispatch 'pi arg))

;;;###autoload
(defun envoy-tmux-org-heading-codex (&optional arg)
  "Hand the org heading at point to Codex in a tmux window.
With a prefix argument ARG, ask first for what its :variable names."
  (interactive "P")
  (envoy-tmux--dispatch 'codex arg))
 
;;;###autoload
(defun envoy-tmux-org-heading-omp (&optional arg)
  "Hand the org heading at point to OMP in a tmux window.
With a prefix argument ARG, ask first for Single model, Plan into or Drive."
  (interactive "P")
  (envoy-tmux--dispatch 'omp arg))


;;;###autoload
(defun envoy-tmux-setup-keys ()
  "Bind the tmux commands in `org-mode-map'.
Not done for you, for the reason `envoy-org-setup-keys' is not.

These five uppercase shortcuts use org commands.  `key-binding' returns nil
before binding, but lowercase fallback still runs.  The `O' shortcut would run
`org-toggle-ordered-property'.  The `C' shortcut would run
`org-clone-subtree-with-time-shift', and the `A' shortcut would run
`org-archive-to-archive-sibling'.  These results came from pressing each
sequence.  A binding takes the shortcut.  The lowercase key keeps working."
  (interactive)
  (require 'org)
  (define-key org-mode-map (kbd "C-c C-x A") #'envoy-tmux-org-heading-claude)
  (define-key org-mode-map (kbd "C-c C-x R") #'envoy-tmux-org-heading-reasonix)
  (define-key org-mode-map (kbd "C-c C-x P") #'envoy-tmux-org-heading-pi)
  (define-key org-mode-map (kbd "C-c C-x O") #'envoy-tmux-org-heading-omp)
  (define-key org-mode-map (kbd "C-c C-x C") #'envoy-tmux-org-heading-codex))

(provide 'envoy-tmux)

;;; envoy-tmux.el ends here
