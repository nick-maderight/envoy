;;; envoy-process.el --- Run a coding agent as a subprocess  -*- lexical-binding: t; -*-

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

;; The transport layer: start an agent, feed it a prompt, collect what it
;; says.  Nothing here knows about org headings or about regions.
;;
;; Three facts about the agent programs shape all of it.
;;
;; They put their answer on standard output and their warnings on
;; standard error, so the two streams are kept apart with a pipe process.
;; Whether a given run warns at all depends on the machine: reasonix was
;; measured emitting a wall of "warning: skill ... has no description"
;; when a locally installed skill lacked a description, and emitting
;; nothing once those were fixed.  So a run that merged the streams works
;; until someone installs a skill, and then hands the JSON reader prose.
;;
;; Their prompt goes in on standard input.  It can hold quotes, newlines,
;; backticks and a hundred kilobytes of org text without a word of
;; quoting, because no shell ever sees it.
;;
;; They report failure in two places and lie in a third.  A non-zero exit
;; status and the envelope's `is_error' are both trustworthy; `subtype'
;; is not, and says "success" on an HTTP 400.  Exit 0 with nothing
;; parseable on standard output is a third state, produced by a rejected
;; command line, and it is a failure too.
;;
;; dsh keeps its own account.  A headless run writes one JSON event per
;; line, and the closing event carries the answer and how the turn ended.
;; A provider names that shape with `:format', and it is read by its own
;; branch rather than bent into the envelope above, since the field names
;; and the notion of success are its own.
;;
;; One fact about Emacs shapes the rest.  A process sentinel is not
;; something Emacs can be relied on to deliver: an open bug (debbugs
;; #63078, #68792) drops it when a child exits very soon after writing
;; very little, which is the shape of a rejected command line or a
;; one-line answer.  Measured on the 27.1 floor it goes missing in most
;; runs.  So `envoy-run' watches the process status as well, and the
;; first of the two to notice finishes the run.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
;; The reader used where Emacs has no native JSON.  Required outright rather
;; than at compile time only: `envoy--parse-json' binds `json-object-type'
;; and its neighbours, which have to be known as special variables, and
;; calls `json-read-from-string', which has to be defined when it runs.
;; json.el has been bundled since Emacs 23, so this costs nothing.
(require 'json)

(defgroup envoy nil
  "Delegate work to a coding agent."
  :group 'tools
  :prefix "envoy-")

(defcustom envoy-provider 'claude
  "Coding agent to send work to.
One of the keys of `envoy-providers'."
  :type 'symbol
  :group 'envoy)

(defcustom envoy-providers
  '((claude
     :program "claude"
     :name "Claude Code"
     :args ("-p" "--output-format" "json")
     :edit-args ("--permission-mode" "acceptEdits")
     :deny-arg "--disallowed-tools"
     :deny-separate t
     :model-arg "--model")
    (reasonix
     :program "reasonix"
     :name "reasonix"
     :args ("-p" "--output-format" "json")
     :edit-args ()
     :deny-arg nil
     :deny-separate nil
     :model-arg "--model")
    (dsh
     :program "dsh"
     :name "dsh"
     ;; The task arrives on standard input, which `-` as the only positional
     ;; asks for.  `--json` is what makes a run report how it ended;
     ;; without it the final message is all that comes back.  `never` is not
     ;; a preference: nobody is at this run's terminal to answer an approval
     ;; card, so a run that asks one would sit there until it timed out.
     :args ("--profile" "headless" "--json" "--approval-policy" "never"
            "--permission-mode" "read-only" "-")
     ;; Written after `:args` on purpose.  A repeated option is a
     ;; replacement in commander, so this is the mode in force for a run
     ;; that may edit, and the read-only mode above is what a run that may
     ;; not gets.  A dsh rewrite writes only under the directory it runs in,
     ;; because that directory is the sandbox root a headless run grants.
     :edit-args ("--permission-mode" "workspace-write")
     :deny-arg nil
     :deny-separate nil
     ;; A headless run takes its model from its profile; there is no option
     ;; to pass one on the command line.
     :model-arg nil
     :format dsh
     :environment ()))
  "Coding agents envoy knows how to run.
Each entry is (SYMBOL . PLIST).  The properties are:

:program     the executable, looked up on the variable `exec-path'
:name        how to say it in a message to the user
:args        arguments that select non-interactive JSON output
:edit-args   further arguments needed before the agent may edit a file
:deny-arg    option that removes tools, or nil if the agent has none
:deny-separate
             non-nil when :deny-arg takes each tool as its own argument
             rather than one comma-separated argument
:model-arg   option that selects a model
:format      how the agent reports the run: `json' for the result envelope
             Claude Code and reasonix print, `dsh' for the closing event of
             a dsh headless run.  Defaults to `json'
:environment
             an alist of (VARIABLE . COMMAND).  COMMAND is run in a shell
             before the agent starts and its trimmed output becomes
             VARIABLE in the agent's environment, which is otherwise this
             Emacs's own.  A credential Emacs does not hold goes here.  A
             command that fails or prints nothing stops the run, and the
             value is never put in a message

An agent whose :deny-arg is nil cannot be stopped from running shell
commands.  See `envoy-deny-tools'."
  :type '(alist :key-type symbol :value-type plist)
  :group 'envoy)

(defcustom envoy-model nil
  "Model to ask the agent for, or nil for whatever it uses by default."
  :type '(choice (const :tag "Agent default" nil) string)
  :group 'envoy)

(defcustom envoy-deny-tools '("Bash")
  "Tools the agent may not use during a rewrite.
Passed through the provider's :deny-arg.

Measured, and the reason this is a deny list rather than an allow list:
naming Read and Edit to Claude Code's --allowed-tools did not stop it
from running a shell command, and neither did reasonix's own
--allowed-tools.  Only --disallowed-tools actually took the tool away.
An allow list here would read like a guarantee and not be one.

A provider with no deny option ignores this, and a rewrite through it
can run commands.  `envoy-rewrite' says so before it starts."
  :type '(repeat string)
  :group 'envoy)

(defcustom envoy-timeout 600
  "Seconds to let a run continue before giving up on it.
The agent is asked to stop, and whatever it already wrote to the file
stays written.  See `envoy-rewrite' for what happens next."
  :type 'integer
  :group 'envoy)

(defun envoy--provider (&optional provider)
  "Return the plist for PROVIDER, or for `envoy-provider'."
  (let* ((key (or provider envoy-provider))
         (entry (assq key envoy-providers)))
    (unless entry
      (user-error "No such envoy provider: %s" key))
    (cdr entry)))

(defun envoy-provider-name (&optional provider)
  "Return the human-readable name of PROVIDER, or of `envoy-provider'."
  (or (plist-get (envoy--provider provider) :name)
      (symbol-name (or provider envoy-provider))))

(defun envoy--program (&optional provider)
  "Return the absolute path to PROVIDER's program.
Signals a `user-error' naming the provider when it is not installed,
which is friendlier than the `file-missing' that `make-process' would
raise several frames later."
  (let* ((plist (envoy--provider provider))
         (program (plist-get plist :program)))
    (or (executable-find program)
        (user-error "Envoy: %s needs `%s', which is not on `exec-path'"
                    (envoy-provider-name provider) program))))

(defun envoy--deny-args (plist)
  "Return the arguments that deny `envoy-deny-tools' for PLIST."
  (let ((option (plist-get plist :deny-arg)))
    (when (and option envoy-deny-tools)
      (if (plist-get plist :deny-separate)
          (cons option envoy-deny-tools)
        (list option (mapconcat #'identity envoy-deny-tools ","))))))

(defun envoy--command (&optional provider edit)
  "Return the command line for PROVIDER as a list.
With EDIT non-nil, include the arguments that let the agent write to
files, and the arguments that deny `envoy-deny-tools'."
  (let ((plist (envoy--provider provider)))
    (append (list (envoy--program provider))
            (plist-get plist :args)
            (when edit (plist-get plist :edit-args))
            (when edit (envoy--deny-args plist))
            (when (and envoy-model (plist-get plist :model-arg))
              (list (plist-get plist :model-arg) envoy-model)))))

(defun envoy-can-deny-tools-p (&optional provider)
  "Return non-nil when PROVIDER can be told not to use a tool."
  (and (plist-get (envoy--provider provider) :deny-arg) t))

(defun envoy--environment (plist)
  "Return the extra environment entries PLIST asks for.
Each is a pair of a variable name and a shell command.  The command is
run now and its trimmed output becomes the variable's value for the
agent, keyed the way `process-environment' wants it.  A command that
fails or prints nothing is a `user-error' naming the variable and not
its value, because a run that starts without a credential it needs only
fails later, inside the agent, where the reason is much harder to see."
  (let (entries)
    (dolist (spec (plist-get plist :environment))
      (let ((variable (car spec))
            (command (cdr spec))
            value)
        (unless (and (stringp variable)
                     (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*\\'" variable))
          (user-error "Envoy: %S is not an environment variable name" variable))
        (setq value
              (with-temp-buffer
                (let ((status (call-process-shell-command command nil t)))
                  (unless (and (integerp status) (zerop status))
                    (user-error
                     "Envoy: the command providing %s failed (%s)"
                     variable status))
                  (string-trim (buffer-string)))))
        (when (string-empty-p value)
          (user-error "Envoy: the command providing %s printed nothing"
                      variable))
        (push (concat variable "=" value) entries)))
    (nreverse entries)))

(defun envoy--parse-json (string)
  "Parse STRING as JSON into an alist, or return nil.
Absent values come back as nil rather than as a sentinel, so a caller
can ask `is_error' directly instead of comparing against :false.

`json-parse-string' is used where Emacs has it and `json-read-from-string'
otherwise; the fallback matters at the 27.1 floor, where an Emacs built
without libjansson has no native reader at all."
  (condition-case nil
      (if (fboundp 'json-parse-string)
          (json-parse-string string
                             :object-type 'alist
                             :array-type 'list
                             :null-object nil
                             :false-object nil)
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-null nil)
              (json-false nil))
          (json-read-from-string string)))
    (error nil)))

(defun envoy--result-envelope-p (envelope)
  "Return non-nil when ENVELOPE has the fields of an agent result."
  (and (listp envelope)
       (cl-every (lambda (entry)
                   (and (consp entry)
                        (symbolp (car entry))))
                 envelope)
       (or (not (assq 'type envelope))
           (equal (cdr (assq 'type envelope)) "result"))
       (assq 'is_error envelope)
       (let ((result (assq 'result envelope)))
         (and result (stringp (cdr result))))))

(defun envoy--parse-envelope (stdout)
  "Return the result envelope in STDOUT as an alist, or nil.
The whole of STDOUT is tried first.  Failing that, the last line that
parses on its own is taken, which is what arrives when the agent has
printed anything ahead of its envelope."
  (let ((envelope
         (or (envoy--parse-json (string-trim stdout))
             (let ((lines (nreverse (split-string stdout "\n" t)))
                   (found nil))
               (while (and lines
                           (not (envoy--result-envelope-p found)))
                 (setq found (envoy--parse-json (string-trim (pop lines)))))
               found))))
    (when (envoy--result-envelope-p envelope)
      envelope)))

(cl-defstruct (envoy-result (:constructor envoy--make-result))
  "What a finished run amounts to.

OK        non-nil when the agent completed its work
TEXT      the agent's closing message, or the reason it failed
STATUS    the exit status of the process
SESSION   the agent's session identifier, for a follow-up turn
COST      what the run cost, in dollars, or nil
TURNS     how many turns the agent took, or nil
STDERR    everything the agent wrote to standard error
STDOUT    everything it wrote to standard output"
  ok text status session cost turns stderr stdout)

(defun envoy--dsh-final-p (event)
  "Return non-nil when EVENT is the closing event of a dsh run."
  (and (listp event)
       (cl-every (lambda (entry)
                   (and (consp entry) (symbolp (car entry))))
                 event)
       (equal (cdr (assq 'type event)) "final")
       (stringp (cdr (assq 'text event)))))

(defun envoy--parse-dsh-final (stdout)
  "Return the closing `final' event in STDOUT as an alist, or nil.
A dsh headless run writes one JSON event per line and ends with that
event.  Tool results and reasoning fill the lines before it, so only a
line that is itself the closing event is taken, and the last one wins."
  (let ((lines (nreverse (split-string stdout "\n" t)))
        (found nil))
    (while (and lines (not found))
      (let ((event (envoy--parse-json (string-trim (pop lines)))))
        (when (envoy--dsh-final-p event)
          (setq found event))))
    found))

(defun envoy--dsh-result-from (status stdout stderr)
  "Interpret a finished dsh headless run and return an `envoy-result'.
STATUS is the process exit status, STDOUT and STDERR its two streams.

A run is good only when it exited 0 and its closing event says the turn
ended `completed'.  Every other ending carries a reason: `error' holds
the agent's own error there, and the rest say only what happened, so the
closing message stands in for them.  A run with no closing event never
got far enough to have one, and the last thing it said on standard error
is the only account of why."
  (let* ((event (envoy--parse-dsh-final stdout))
         (text (cdr (assq 'text event)))
         (reason (cdr (assq 'reason event)))
         (kind (cdr (assq 'kind (and (listp reason) reason))))
         (failure (cdr (assq 'error (and (listp reason) reason))))
         (message (cdr (assq 'message (and (listp failure) failure)))))
    (cond
     ((null event)
      (envoy--make-result
       :ok nil :status status :stdout stdout :stderr stderr
       :text (if (string-empty-p (string-trim stderr))
                 (format "The agent exited with status %s and said nothing" status)
               (car (last (split-string (string-trim stderr) "\n" t))))))
     ((and (zerop status) (equal kind "completed"))
      (envoy--make-result
       :ok t :status status :stdout stdout :stderr stderr
       :text (or text "")
       :session (cdr (assq 'sessionId event))))
     (t
      (envoy--make-result
       :ok nil :status status :stdout stdout :stderr stderr
       :text (or (and (stringp text) (not (string-empty-p text)) text)
                 message
                 (format "The agent exited with status %s%s" status
                         (if kind (format " (%s)" kind) "")))
       :session (cdr (assq 'sessionId event)))))))

(defun envoy--result-from (status stdout stderr &optional format)
  "Interpret a finished run and return an `envoy-result'.
STATUS is the process exit status, STDOUT and STDERR its two streams.
FORMAT is the provider's `:format', defaulting to `json'.

Failure is read from STATUS and from the envelope's `is_error', never
from its `subtype': Claude Code reports subtype \"success\" alongside
is_error true and an HTTP 400, so a reader that trusted subtype would
call a failed run good.  Both agents put something a person can read in
`result', so that is the message either way.

Exit 0 with no envelope is its own failure, and the one a rejected
command line produces: the agent prints its usage to standard error and
leaves standard output empty.

A `dsh' run reports in newline-delimited events instead, and its own
reader maps the closing one onto the same fields; see
`envoy--dsh-result-from'."
  (if (eq format 'dsh)
      (envoy--dsh-result-from status stdout stderr)
    (let* ((envelope (envoy--parse-envelope stdout))
           (text (cdr (assq 'result envelope)))
           (errored (cdr (assq 'is_error envelope))))
      (cond
       ((null envelope)
        (envoy--make-result
         :ok nil :status status :stdout stdout :stderr stderr
         :text (if (string-empty-p (string-trim stderr))
                   (format "The agent exited with status %s and said nothing" status)
                 (car (last (split-string (string-trim stderr) "\n" t))))))
       ((or (not (zerop status)) errored)
        (envoy--make-result
         :ok nil :status status :stdout stdout :stderr stderr
         :text (or text (format "The agent exited with status %s" status))
         :session (cdr (assq 'session_id envelope))))
       (t
        (envoy--make-result
         :ok t :status status :stdout stdout :stderr stderr
         :text (or text "")
         :session (cdr (assq 'session_id envelope))
         :cost (cdr (assq 'total_cost_usd envelope))
         :turns (cdr (assq 'num_turns envelope))))))))

(defun envoy--check-brief (brief)
  "Refuse a dispatch whose brief BRIEF carries no task, by signaling a user error.
A brief that renders to nothing, or to only the placeholder word that
stands where the task text belongs, still starts an agent and bills a
run, and the run reports status lines for work that does not exist.
Both transports call this before a process starts or a prompt file is
written: `envoy-run' and `envoy-tmux-send'."
  (let ((text (string-trim (or brief ""))))
    (cond
     ((string-empty-p text)
      (user-error "Envoy: the rendered brief is empty; no agent was started"))
     ((and (= (length text) 5)
           (equal (downcase text) "brief"))
      (user-error "Envoy: the rendered brief is only the placeholder word \"brief\"; no agent was started")))))

(defun envoy-run (prompt callback &rest keys)
  "Send PROMPT to a coding agent and call CALLBACK with the result.
CALLBACK receives one `envoy-result'.  Return the process, which
`envoy-cancel-process' can stop.

KEYS are keyword arguments:

:provider   which agent to use, defaulting to `envoy-provider'
:edit       non-nil to let the agent write to files
:directory  the directory to run in, which is what the agent treats as
            the project, and so decides which files it can find
:timeout    seconds to wait, defaulting to `envoy-timeout'"
  (envoy--check-brief prompt)
  (let* ((provider (plist-get keys :provider))
         (directory (or (plist-get keys :directory) default-directory))
         (timeout (or (plist-get keys :timeout) envoy-timeout))
         stdout stderr-buffer stderr process timer watchdog finish cleanup
         command environment)
    (setq cleanup
          (lambda ()
            (when process
              (process-put process 'envoy-finished t)
              (when (process-live-p process)
                (delete-process process)))
            (when timer (cancel-timer timer))
            (when watchdog (cancel-timer watchdog))
            (when (process-live-p stderr) (delete-process stderr))
            (when (buffer-live-p stdout) (kill-buffer stdout))
            (when (buffer-live-p stderr-buffer) (kill-buffer stderr-buffer))))
    (setq finish
          (lambda (proc)
            ;; Called from the sentinel, or from the watchdog when Emacs
            ;; never delivers one.  Whichever arrives first does the work
            ;; and the other finds nothing left to do.
            (unless (process-get proc 'envoy-finished)
              (process-put proc 'envoy-finished t)
              (when timer (cancel-timer timer))
              (when watchdog (cancel-timer watchdog))
              ;; Both pipes can still hold output when the child has gone,
              ;; so they are drained rather than read once.
              (while (accept-process-output proc 0.05))
              (while (accept-process-output stderr 0.05))
              (let ((out (with-current-buffer stdout (buffer-string)))
                    (err (with-current-buffer stderr-buffer (buffer-string)))
                    (status (process-exit-status proc)))
                (kill-buffer stdout)
                (when (process-live-p stderr) (delete-process stderr))
                (kill-buffer stderr-buffer)
                (funcall callback
                         (if (process-get proc 'envoy-cancelled)
                             (envoy--make-result
                              :ok nil :status status :stdout out :stderr err
                              :text (if (process-get proc 'envoy-timed-out)
                                        (format "Gave up after %s seconds"
                                                timeout)
                                      "Cancelled"))
                           (envoy--result-from
                            status out err
                            (plist-get (envoy--provider provider) :format))))))))
    (condition-case error
        (progn
          (setq command (envoy--command provider (plist-get keys :edit)))
          (setq environment (envoy--environment (envoy--provider provider)))
          (setq stdout (generate-new-buffer " *envoy-stdout*"))
          (setq stderr-buffer (generate-new-buffer " *envoy-stderr*"))
          ;; A pipe process of its own for standard error.  Without it the
          ;; agent's warnings land in the same buffer as its JSON and the
          ;; envelope no longer parses.
          (setq stderr (make-pipe-process :name " *envoy-stderr*"
                                          :buffer stderr-buffer
                                          :coding 'utf-8-unix
                                          :noquery t))
          (setq process
                ;; The agent inherits this directory, and it is the whole of
                ;; what the agent treats as the project: which files it can
                ;; find, and which configuration it reads, both follow from
                ;; it.  The environment is this Emacs's own, with whatever
                ;; the provider asked for put in front of it so its entries
                ;; win over an inherited value of the same name.
                (let ((default-directory directory)
                      (process-environment (append environment
                                                   process-environment)))
                  (make-process
                   :name "envoy"
                   :buffer stdout
                   :command command
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :noquery t
                   :stderr stderr
                   :sentinel
                   (lambda (proc event)
                     (ignore event)
                     (when (memq (process-status proc) '(exit signal))
                       (funcall finish proc))))))
          ;; A sentinel is not something Emacs can be relied on to deliver.
          ;; An open bug (debbugs #63078, #68792) drops it when a child exits
          ;; very soon after writing very little, which is exactly the shape
          ;; of a rejected command line or a one-line answer, and it is
          ;; reproducible on the 27.1 floor in most runs.  Left to the
          ;; sentinel alone the callback would never come, and a caller
          ;; waiting on it would wait for good.  So the process status is
          ;; watched as well, and whichever notices first finishes the run.
          (setq watchdog
                (run-at-time 0.2 0.2
                             (lambda ()
                               (when (memq (process-status process)
                                           '(exit signal))
                                 (funcall finish process)))))
          (process-put process 'envoy-stderr stderr)
          (process-put process 'envoy-directory directory)
          (when (and timeout (> timeout 0))
            (setq timer
                  (run-at-time timeout nil
                               (lambda ()
                                 (when (process-live-p process)
                                   (process-put process 'envoy-timed-out t)
                                   (envoy-cancel-process process))))))
          ;; The prompt goes in whole and the stream is closed, which is how
          ;; the agent knows to start.  Measured intact at 216 kilobytes.
          (process-send-string process prompt)
          (process-send-eof process)
          process)
      (error
       (let ((result
              (envoy--make-result
               :ok nil
               :text (error-message-string error)
               :stdout (and (buffer-live-p stdout)
                            (with-current-buffer stdout
                              (buffer-string)))
               :stderr (and (buffer-live-p stderr-buffer)
                            (with-current-buffer stderr-buffer
                              (buffer-string))))))
         (funcall cleanup)
         (if process
             (progn
               (funcall callback result)
               process)
           (funcall callback result)
           (signal (car error) (cdr error))))))))

(defun envoy-cancel-process (process)
  "Stop PROCESS, marking the run cancelled so its callback can tell.
Whatever the agent already wrote to disk stays written: each edit it
makes is a complete write of its own, and the ones that landed before
the signal landed for good.  A caller that gave the agent a file to edit
has to be able to put that file back."
  (when (process-live-p process)
    (process-put process 'envoy-cancelled t)
    (interrupt-process process)
    ;; A second, harder signal for an agent that ignores the first.
    (run-at-time 2 nil (lambda ()
                         (when (process-live-p process)
                           (delete-process process))))))

(provide 'envoy-process)

;;; envoy-process.el ends here
