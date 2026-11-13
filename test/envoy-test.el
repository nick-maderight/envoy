;;; envoy-test.el --- Tests for envoy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Nick

;; Author: Nick <nick@maderightsoftware.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; No test here runs a real agent.  Two reasons: a real run costs money and
;; needs a network, and its output is different every time, so a suite built on
;; one would be both expensive and unreliable.
;;
;; The runs that do need a process use a small shell script standing in for the
;; agent, spawned through the same `make-process' the package uses.  That
;; exercises the plumbing that actually breaks -- the two streams staying apart,
;; the prompt arriving on standard input, the exit status coming back through the
;; sentinel -- against a script that writes a warning to standard error the way
;; reasonix does.
;;
;; Everything else calls the functions directly.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'envoy)
(require 'envoy-process)
(require 'envoy-review)
(require 'envoy-org)
(require 'org-element)
(require 'org-agenda)
(require 'envoy-tmux)
(require 'envoy-notmuch)
(require 'seq)

;;; Standing in for an agent

(defvar envoy-test--scripts nil
  "Scripts written by `envoy-test--agent', deleted after each test.")

(defun envoy-test--agent (envelope &optional status stderr-line input-file)
  "Write a script that behaves like an agent and return its file name.
It reads its whole standard input and discards it, writes STDERR-LINE to
standard error, prints ENVELOPE on standard output and exits STATUS.
When INPUT-FILE is non-nil, the script writes standard input there instead."
  (let ((script (make-temp-file "envoy-test-agent-" nil ".sh")))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              (if input-file
                  (format "cat > %s\n" (shell-quote-argument input-file))
                "cat >/dev/null\n")
              (format "echo '%s' 1>&2\n"
                      (or stderr-line "warning: skill \"x\" has no description"))
              (format "printf '%%s\\n' '%s'\n" envelope)
              (format "exit %d\n" (or status 0))))
    (set-file-modes script #o755)
    (push script envoy-test--scripts)
    script))

(defun envoy-test--stdin-agent ()
  "Write a script that reports how many bytes of standard input it read."
  (let ((script (make-temp-file "envoy-test-stdin-" nil ".sh")))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              "n=$(wc -c | tr -d ' ')\n"
              "printf '{\"type\":\"result\",\"is_error\":false,\"result\":\"%s\"}\\n' \"$n\"\n"))
    (set-file-modes script #o755)
    (push script envoy-test--scripts)
    script))

(defmacro envoy-test--with-agent (script &rest body)
  "Evaluate BODY with `envoy-providers' holding one agent, SCRIPT."
  (declare (indent 1))
  `(let ((envoy-providers (list (list 'test
                                      :program ,script
                                      :name "test agent"
                                      :args nil
                                      :edit-args nil
                                      :deny-arg nil
                                      :model-arg "--model")))
         (envoy-provider 'test)
         (envoy-model nil))
     (unwind-protect (progn ,@body)
       (dolist (file envoy-test--scripts)
         (when (file-exists-p file) (delete-file file)))
       (setq envoy-test--scripts nil))))

(defmacro envoy-test--without-native-json (&rest body)
  "Evaluate BODY in an Emacs that has no `json-parse-string'.
Stubbing the function would not do: the reader asks `fboundp' before it
calls anything, so the native branch would still be the one that runs."
  `(let ((definition (and (fboundp 'json-parse-string)
                          (symbol-function 'json-parse-string))))
     (unwind-protect
         (progn (when definition (fmakunbound 'json-parse-string))
                ,@body)
       (when definition (fset 'json-parse-string definition)))))

(defun envoy-test--run (prompt &rest keys)
  "Send PROMPT through `envoy-run' and return the result, once it arrives.
KEYS are passed through.  Waits for the sentinel rather than sleeping a
fixed time, so the test is as fast as the process and never flaky."
  (let (result)
    (let ((process (apply #'envoy-run prompt
                          (lambda (r) (setq result r))
                          keys)))
      (with-timeout (10 (error "Envoy-test: the agent never finished"))
        (while (not result)
          (accept-process-output process 0.05)))
      result)))

;;; The command line

(ert-deftest envoy-test-command-includes-edit-arguments ()
  "Editing arguments appear only when the agent is allowed to edit."
  (let ((envoy-providers '((x :program "sh" :name "x"
                              :args ("-p") :edit-args ("--may-edit")
                              :deny-arg nil :model-arg "--model")))
        (envoy-provider 'x)
        (envoy-model nil))
    (should-not (member "--may-edit" (envoy--command nil nil)))
    (should (member "--may-edit" (envoy--command nil t)))))

(ert-deftest envoy-test-deny-arguments-shape ()
  "A deny option is passed the way its agent expects it."
  (let ((envoy-deny-tools '("Bash" "Write")))
    ;; One argument per tool.
    (should (equal (envoy--deny-args '(:deny-arg "--no" :deny-separate t))
                   '("--no" "Bash" "Write")))
    ;; One argument, comma separated.
    (should (equal (envoy--deny-args '(:deny-arg "--no" :deny-separate nil))
                   '("--no" "Bash,Write")))
    ;; An agent with no such option gets nothing, rather than a flag it
    ;; would reject.
    (should-not (envoy--deny-args '(:deny-arg nil)))))

(ert-deftest envoy-test-deny-arguments-omitted-when-nothing-denied ()
  "No deny option is passed when nothing is being denied."
  (let ((envoy-deny-tools nil))
    (should-not (envoy--deny-args '(:deny-arg "--no" :deny-separate t)))))

(ert-deftest envoy-test-missing-program-is-reported-by-name ()
  "A missing agent is a message naming it, not a `file-missing'."
  (let ((envoy-providers '((gone :program "envoy-no-such-program-xyz"
                                 :name "Gone" :args nil :edit-args nil
                                 :deny-arg nil :model-arg "--model")))
        (envoy-provider 'gone))
    (let ((error-message
           (cadr (should-error (envoy--program) :type 'user-error))))
      (should (string-match-p "Gone" error-message))
      (should (string-match-p "envoy-no-such-program-xyz" error-message)))))

(ert-deftest envoy-test-unknown-provider-is-reported ()
  "Asking for an agent that is not in the table says so."
  (should-error (envoy--provider 'nothing-like-this) :type 'user-error))

;;; Reading what the agent said

(ert-deftest envoy-test-envelope-is-read ()
  "A successful envelope yields its text, cost and turns."
  (let ((result (envoy--result-from
                 0 "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\
\"result\":\"Shortened the paragraph.\",\"session_id\":\"abc\",\
\"total_cost_usd\":0.03,\"num_turns\":2}" "")))
    (should (envoy-result-ok result))
    (should (equal (envoy-result-text result) "Shortened the paragraph."))
    (should (equal (envoy-result-session result) "abc"))
    (should (equal (envoy-result-turns result) 2))))

(ert-deftest envoy-test-is-error-beats-subtype-success ()
  "An envelope claiming success while reporting an error is a failure.

Measured against Claude Code with an invalid model: it answers is_error
true, api_error_status 400, and subtype \"success\".  A reader that
trusted subtype would call that run good and show the API error as
though it were the agent's summary of its work.

The exit status here is 0, which the real run does not do -- it exits 1.
That is on purpose: with a non-zero status in the envelope the test would
pass on the status alone, whether or not the code ever looked at
is_error, and the thing being pinned down is that it looks at is_error."
  (let ((result (envoy--result-from
                 0 "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":true,\
\"result\":\"API Error: 400 the model is invalid\",\"api_error_status\":400}" "")))
    (should-not (envoy-result-ok result))
    (should (string-match-p "400" (envoy-result-text result))))
  ;; And the pairing the real failure arrives as: both signals set.
  (should-not (envoy-result-ok
               (envoy--result-from
                1 "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":true,\
\"result\":\"API Error: 400 the model is invalid\"}" ""))))

(ert-deftest envoy-test-nonzero-status-is-failure ()
  "A non-zero exit status is a failure whatever the envelope claims."
  (let ((result (envoy--result-from
                 1 "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\
\"result\":\"all fine\"}" "")))
    (should-not (envoy-result-ok result))))

(ert-deftest envoy-test-exit-zero-without-envelope-is-failure ()
  "Exit 0 with nothing parseable is a failure, and says what went wrong.

This is what a rejected command line looks like: the agent prints its
usage to standard error and leaves standard output empty."
  (let ((result (envoy--result-from 0 "" "error: unknown option --bogus")))
    (should-not (envoy-result-ok result))
    (should (string-match-p "unknown option" (envoy-result-text result)))))

(ert-deftest envoy-test-envelope-found-after-other-output ()
  "An envelope is still found when the agent printed something first."
  (let ((result (envoy--result-from
                 0 "loading configuration\n{\"type\":\"result\",\"is_error\":false,\
\"result\":\"done\"}" "")))
    (should (envoy-result-ok result))
    (should (equal (envoy-result-text result) "done"))))
(ert-deftest envoy-test-envelope-skips-trailing-json-object ()
  "A trailing JSON object does not hide the earlier result envelope."
  (let ((result (envoy--result-from
                 0 "loading configuration\n{\"type\":\"result\",\"is_error\":false,\
\"result\":\"done\"}\n{\"status\":\"done\"}" "")))
    (should (envoy-result-ok result))
    (should (equal (envoy-result-text result) "done"))))

(ert-deftest envoy-test-absent-values-are-nil ()
  "A JSON false arrives as nil, not as a sentinel that looks true."
  (let ((parsed (envoy--parse-json "{\"a\":false,\"b\":null,\"c\":1}")))
    (should-not (cdr (assq 'a parsed)))
    (should-not (cdr (assq 'b parsed)))
    (should (equal (cdr (assq 'c parsed)) 1))))

(ert-deftest envoy-test-json-fallback-reader-agrees ()
  "The fallback reader gives the same answer as the native one.

Emacs built without libjansson has no `json-parse-string' at all, and at
the 27.1 floor that is a real configuration rather than a hypothetical
one, so both branches have to agree."
  (let ((text "{\"result\":\"ok\",\"is_error\":false,\"n\":2}"))
    (let ((native (envoy--parse-json text))
          (fallback (envoy-test--without-native-json
                     (envoy--parse-json text))))
      (should (equal (cdr (assq 'result native)) "ok"))
      (should (equal (cdr (assq 'result fallback)) "ok"))
      (should (equal (cdr (assq 'is_error native))
                     (cdr (assq 'is_error fallback))))
      (should (equal (cdr (assq 'n native)) (cdr (assq 'n fallback)))))))

(ert-deftest envoy-test-malformed-json-is-nil-not-an-error ()
  "Output that is not JSON is nil rather than a signal."
  (should-not (envoy--parse-json "not json at all"))
  (should-not (envoy--parse-json "")))

;;; A dsh headless run

(ert-deftest envoy-test-dsh-provider-asks-for-a-headless-run ()
  "The shipped dsh entry runs one headless task that cannot stop to ask.
A run with nobody at its terminal must never raise an approval card,
which is why the policy travels on every command line rather than being
left to a profile.  The mode that matters is written last: commander
replaces a repeated option, so the read-only mode is what a caller that
may not edit gets, and the editable mode comes after it."
  (let ((envoy-model "a-model-nobody-can-pass"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (program)
                 (and (equal program "dsh") "/usr/bin/dsh"))))
      (let ((line (envoy--command 'dsh nil)))
        (should (equal (car line) "/usr/bin/dsh"))
        (should (equal (list (nth 1 line) (nth 2 line))
                       '("--profile" "headless")))
        (should (member "--json" line))
        (should (equal (cadr (member "--approval-policy" line)) "never"))
        (should (member "read-only" line))
        (should-not (member "workspace-write" line))
        ;; The task arrives on standard input.
        (should (equal (car (last line)) "-"))
        ;; A headless run takes its model from its profile, and a model
        ;; option here would be rejected rather than honoured.
        (should-not (member "--model" line))
        (should-not (member envoy-model line)))
      (let ((line (envoy--command 'dsh t)))
        (should (member "workspace-write" line))
        (should (member "read-only" line))
        (should (< (seq-position line "read-only" (lambda (a b) (equal a b)))
                   (seq-position line "workspace-write" (lambda (a b) (equal a b)))))))
    ;; It is a provider like any other, and a rewrite through it says so.
    (should (assq 'dsh envoy-providers))
    (should-not (envoy-can-deny-tools-p 'dsh))))

(ert-deftest envoy-test-dsh-final-event-is-read ()
  "The closing event of a dsh run is read for its text, session and ending.
The lines before it carry status, reasoning and tool results, so a
reader that took the first JSON line would take a status for an answer."
  (let ((result (envoy--result-from
                 0 (concat "{\"type\":\"status\",\"phase\":\"turn_start\",\"turn\":1}\n"
                           "{\"type\":\"tool_call\",\"callId\":\"c1\",\"tool\":\"edit\"}\n"
                           "{\"type\":\"final\",\"sessionId\":\"session-1\",\
\"reason\":{\"kind\":\"completed\"},\"text\":\"Rewrote the paragraph.\"}\n")
                 "" 'dsh)))
    (should (envoy-result-ok result))
    (should (equal (envoy-result-text result) "Rewrote the paragraph."))
    (should (equal (envoy-result-session result) "session-1"))))

(ert-deftest envoy-test-dsh-unfinished-turn-is-a-failure ()
  "A run whose turn did not end `completed' is a failure with its reason.
dsh reports a provider error inside the closing event's reason, and that
message is the only account of what went wrong."
  (let ((result (envoy--result-from
                 1 "{\"type\":\"final\",\"sessionId\":\"session-2\",\
\"reason\":{\"kind\":\"error\",\"error\":{\"code\":\"AUTH\",\
\"message\":\"the key was refused\"}},\"text\":\"\"}"
                 "dsh: AUTH: the key was refused\n" 'dsh)))
    (should-not (envoy-result-ok result))
    (should (equal (envoy-result-text result) "the key was refused"))))

(ert-deftest envoy-test-dsh-stop-reason-without-a-message-still-fails ()
  "A run that stopped for a reason dsh names only is still a failure.
`interrupted' and `aborted' carry no message, so the ending itself is
what the person reading the echo area has to be told."
  (let ((result (envoy--result-from
                 1 "{\"type\":\"final\",\"sessionId\":\"s\",\
\"reason\":{\"kind\":\"interrupted\"},\"text\":\"partial answer\"}" "" 'dsh)))
    (should-not (envoy-result-ok result))
    (should (equal (envoy-result-text result) "partial answer")))
  (let ((result (envoy--result-from
                 1 "{\"type\":\"final\",\"sessionId\":\"s\",\
\"reason\":{\"kind\":\"interrupted\"},\"text\":\"\"}" "" 'dsh)))
    (should-not (envoy-result-ok result))
    (should (string-match-p "interrupted" (envoy-result-text result)))))

(ert-deftest envoy-test-dsh-run-with-no-final-event-is-a-failure ()
  "A run that never got far enough to report is read from standard error.
A rejected command line is exactly this shape: the usage text on
standard error, nothing on standard output, and an exit status."
  (let ((result (envoy--result-from 2 "" "error: a task is required\n" 'dsh)))
    (should-not (envoy-result-ok result))
    (should (equal (envoy-result-text result) "error: a task is required")))
  (let ((result (envoy--result-from 0 "" "" 'dsh)))
    (should-not (envoy-result-ok result))
    (should (string-match-p "status 0" (envoy-result-text result)))))

(ert-deftest envoy-test-dsh-completed-reason-cannot-outvote-the-status ()
  "Exit status and the closing reason must both say the run finished.
A crash after the turn closed leaves a `completed' event behind."
  (let ((result (envoy--result-from
                 1 "{\"type\":\"final\",\"sessionId\":\"s\",\
\"reason\":{\"kind\":\"completed\"},\"text\":\"answered\"}" "" 'dsh)))
    (should-not (envoy-result-ok result))))

(ert-deftest envoy-test-provider-environment-is-read-from-a-command ()
  "A provider's environment entries come from its own commands.
This is where a credential Emacs does not hold comes from, so the value
is taken from the command's output and trimmed."
  (should (equal (envoy--environment
                  '(:environment (("ENVOY_TEST_ONE" . "printf ' value '"))))
                 '("ENVOY_TEST_ONE=value")))
  (should (equal (envoy--environment
                  '(:environment (("A" . "printf %s one")
                                  ("B" . "printf 'two=with=equals'"))))
                 '("A=one" "B=two=with=equals")))
  (should-not (envoy--environment '(:environment nil)))
  (should-not (envoy--environment '())))

(ert-deftest envoy-test-provider-environment-failure-names-the-variable ()
  "A credential command that fails or prints nothing stops the run.
Starting anyway would fail later inside the agent, where the reason is
much harder to see.  The message names the variable and never a value."
  (dolist (spec '(("ENVOY_TEST_FAILED" . "exit 3")
                  ("ENVOY_TEST_EMPTY" . "true")))
    (let ((message (cadr (should-error (envoy--environment
                                        (list :environment (list spec)))
                                       :type 'user-error))))
      (should (string-match-p (car spec) message))
      (should-not (string-match-p "=.*" message)))))

(ert-deftest envoy-test-bad-environment-name-is-refused ()
  "An entry whose name is not a variable name is refused before it runs."
  (let ((ran nil))
    (cl-letf (((symbol-function 'call-process-shell-command)
               (lambda (&rest _) (setq ran t) 0)))
      (should-error (envoy--environment '(:environment (("bad name" . "true"))))
                    :type 'user-error)
      (should-not ran))))

(ert-deftest envoy-test-dsh-run-end-to-end ()
  "A dsh-shaped run reaches the caller through the dsh reader.
The stand-in prints a status line before its closing event, the way a
real run does, and reports the credential it was handed."
  (let* ((script (make-temp-file "envoy-test-dsh-" nil ".sh"))
         (envoy-providers (list (list 'test
                                      :program script
                                      :name "test dsh"
                                      :args '("-")
                                      :edit-args nil
                                      :deny-arg nil
                                      :model-arg nil
                                      :format 'dsh
                                      :environment
                                      '(("ENVOY_TEST_DEEPSEEK_KEY"
                                         . "printf %s secret-from-a-command")))))
         (envoy-provider 'test)
         (envoy-model nil))
    (with-temp-file script
      (insert "#!/bin/sh\n"
              "cat >/dev/null\n"
              "printf '%s\\n' '{\"type\":\"status\",\"phase\":\"turn_start\",\"turn\":1}'\n"
              "printf '{\"type\":\"final\",\"sessionId\":\"session-e2e\",\
\"reason\":{\"kind\":\"completed\"},\"text\":\"%s\"}\\n' \
\"${ENVOY_TEST_DEEPSEEK_KEY:-missing}\"\n"))
    (set-file-modes script #o755)
    (unwind-protect
        (let ((result (envoy-test--run "do something")))
          (should (envoy-result-ok result))
          (should (equal (envoy-result-text result) "secret-from-a-command"))
          (should (equal (envoy-result-session result) "session-e2e")))
      (ignore-errors (delete-file script)))))

(ert-deftest envoy-test-environment-failure-starts-nothing ()
  "A run whose credential command failed never starts an agent.
The provider here would otherwise start, so the refusal has to happen
before the process exists rather than being reported afterwards."
  (let ((envoy-providers (list (list 'test
                                     :program "/bin/true"
                                     :name "test"
                                     :args nil
                                     :edit-args nil
                                     :deny-arg nil
                                     :model-arg nil
                                     :environment
                                     '(("ENVOY_TEST_MISSING" . "exit 4")))))
        (envoy-provider 'test)
        started)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (setq started t))))
      (should-error (envoy-run "do something" #'ignore) :type 'user-error))
    (should-not started)))

;;; Running a process

(ert-deftest envoy-test-run-separates-the-two-streams ()
  "The agent's warnings do not reach the JSON reader.

The stand-in writes a warning to standard error on the way out, the way
reasonix does on every run.  Merging the streams would leave the envelope
unparseable and this test failing."
  (let ((script (envoy-test--agent
                 "{\"type\":\"result\",\"is_error\":false,\"result\":\"clean\"}")))
    (envoy-test--with-agent script
      (let ((result (envoy-test--run "do something")))
        (should (envoy-result-ok result))
        (should (equal (envoy-result-text result) "clean"))
        (should-not (string-match-p "warning" (envoy-result-stdout result)))
        (should (string-match-p "warning" (envoy-result-stderr result)))))))

(ert-deftest envoy-test-run-reports-a-nonzero-exit ()
  "An agent that exits non-zero is reported as having failed."
  (let ((script (envoy-test--agent
                 "{\"type\":\"result\",\"is_error\":true,\"result\":\"no\"}" 7)))
    (envoy-test--with-agent script
      (let ((result (envoy-test--run "do something")))
        (should-not (envoy-result-ok result))
        (should (equal (envoy-result-status result) 7))))))

(ert-deftest envoy-test-run-refuses-empty-or-placeholder-brief ()
  "A subprocess dispatch with an empty or placeholder brief starts nothing.
The refusal happens before a process is created, so nothing is billed
for a brief that carries no task."
  (cl-letf (((symbol-function 'make-process)
             (lambda (&rest _)
               (ert-fail "Envoy started a process for a refused brief"))))
    (dolist (brief '("" "  " "brief" "Brief"))
      (should-error (envoy-run brief (lambda (_result)))
                    :type 'user-error))))

(ert-deftest envoy-test-prompt-arrives-whole-on-stdin ()
  "A long prompt reaches the agent intact, and needs no quoting.

The prompt here holds the characters that would end a shell command line
early -- quotes, a dollar sign, a backtick, a newline -- because an org
subtree routinely does, and none of them are escaped anywhere."
  (let* ((script (envoy-test--stdin-agent))
         (awkward "a 'quote' a \"double\" a $(command) a `backtick`\nand a newline\n")
         (prompt (concat awkward (make-string 20000 ?x))))
    (envoy-test--with-agent script
      (let ((result (envoy-test--run prompt)))
        (should (envoy-result-ok result))
        (should (equal (string-to-number (envoy-result-text result))
                       (string-bytes prompt)))))))

(ert-deftest envoy-test-run-calls-back-exactly-once ()
  "A finished run reports itself once, however Emacs notices it finished.

Two things watch for the end of a run, because a sentinel alone goes
missing on the 27.1 floor (debbugs #63078, #68792).  Both are therefore
expected to fire, and only the first of them may do the work: the second
would hand the caller a second result assembled from buffers already
killed, which for `envoy-rewrite' means a second diff for one edit.

Waiting on the count rather than on the first result is the point.  A
test that returned as soon as one arrived would pass whether or not a
second followed it."
  (let ((script (envoy-test--agent
                 "{\"type\":\"result\",\"is_error\":false,\"result\":\"once\"}")))
    (envoy-test--with-agent script
      (let ((calls 0)
            (process nil))
        (setq process (envoy-run "do something"
                                 (lambda (_result) (setq calls (1+ calls)))))
        (with-timeout (10 (error "Envoy-test: the agent never finished"))
          (while (zerop calls)
            (accept-process-output process 0.05)))
        ;; Long enough for the watchdog to come round again, and for a
        ;; late sentinel to arrive after the watchdog has already reported.
        (sleep-for 1)
        (should (equal calls 1))))))

(ert-deftest envoy-test-run-reports-prompt-send-failure-through-callback ()
  "A prompt send failure is delivered as a failed result to the callback."
  (let ((script (envoy-test--agent
                 "{\"type\":\"result\",\"is_error\":false,\"result\":\"unused\"}"))
        (calls 0)
        result)
    (envoy-test--with-agent script
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (&rest _) (error "Send failed"))))
        (envoy-run "do something"
                   (lambda (received)
                     (setq calls (1+ calls)
                           result received))))
      (should (equal calls 1))
      (should (envoy-result-p result))
      (should-not (envoy-result-ok result))
      (should (string-match-p "Send failed" (envoy-result-text result))))))

;;; Snapshot, diff and revert

(ert-deftest envoy-test-snapshot-copies-the-file ()
  "A snapshot holds what the file said before the run."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "first\nsecond\n")))
    (unwind-protect
        (let ((snapshot (envoy-snapshot file)))
          (unwind-protect
              (progn
                (should snapshot)
                (with-temp-buffer
                  (insert-file-contents snapshot)
                  (should (equal (buffer-string) "first\nsecond\n")))
                ;; Changing the file afterwards does not change the copy.
                (with-temp-file file (insert "rewritten\n"))
                (should (envoy--file-changed-p file snapshot))
                (with-temp-buffer
                  (insert-file-contents snapshot)
                  (should (equal (buffer-string) "first\nsecond\n"))))
            (delete-file snapshot)))
      (delete-file file))))

(ert-deftest envoy-test-snapshot-of-a-file-that-does-not-exist ()
  "A file the run will create has no snapshot, and that is not an error."
  (let ((file (expand-file-name "envoy-test-absent.txt"
                                temporary-file-directory)))
    (when (file-exists-p file) (delete-file file))
    (should-not (envoy-snapshot file))))

(ert-deftest envoy-test-unchanged-file-is-recognised ()
  "A run that changed nothing is not reported as a change."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "same\n")))
    (unwind-protect
        (let ((snapshot (envoy-snapshot file)))
          (unwind-protect
              (should-not (envoy--file-changed-p file snapshot))
            (delete-file snapshot)))
      (delete-file file))))

(ert-deftest envoy-test-revert-restores-a-partial-rewrite ()
  "Reverting puts back a file the agent had already half rewritten.

This is the case that makes the snapshot necessary rather than tidy.  An
interrupted agent leaves the edits it finished behind: measured on a file
of thirty lines, an agent stopped after twenty-five seconds had rewritten
eleven of them and those eleven stayed rewritten.  So the file here is
left in exactly that state -- some lines changed, the rest not -- and
reverting has to undo all of it."
  (let* ((original (mapconcat (lambda (n) (format "line %d as written" n))
                              (number-sequence 1 30) "\n"))
         (file (make-temp-file "envoy-test-" nil ".txt"
                               (concat original "\n"))))
    (unwind-protect
        (let ((snapshot (envoy-snapshot file)))
          ;; Eleven lines rewritten, nineteen not.
          (with-temp-file file
            (insert (concat
                     (mapconcat (lambda (n) (format "LINE %d REWRITTEN" n))
                                (number-sequence 1 11) "\n")
                     "\n"
                     (mapconcat (lambda (n) (format "line %d as written" n))
                                (number-sequence 12 30) "\n")
                     "\n")))
          (should (envoy--file-changed-p file snapshot))
          (let ((buffer (get-buffer-create "*envoy-test-review*")))
            (unwind-protect
                (with-current-buffer buffer
                  (setq envoy-review--file file
                        envoy-review--snapshot snapshot)
                  (cl-letf (((symbol-function 'quit-window) #'ignore)
                            ((symbol-function 'kill-buffer) #'ignore))
                    (envoy-review-revert))
                  (with-temp-buffer
                    (insert-file-contents file)
                    (should (equal (buffer-string) (concat original "\n")))))
              (when (buffer-live-p buffer) (kill-buffer buffer)))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest envoy-test-review-of-an-unchanged-file-opens-nothing ()
  "A run that changed nothing opens no review buffer."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "unchanged\n")))
    (unwind-protect
        (let ((snapshot (envoy-snapshot file)))
          (should-not (envoy-review file snapshot "nothing needed doing"))
          ;; The snapshot is cleaned up rather than left in /tmp.
          (should-not (file-exists-p snapshot)))
      (delete-file file))))

(ert-deftest envoy-test-review-shows-the-diff-and-the-keys ()
  "A review buffer holds the diff, the summary and how to act on it."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n")))
    (unwind-protect
        (let* ((snapshot (envoy-snapshot file))
               buffer)
          (with-temp-file file (insert "after\n"))
          (unwind-protect
              (progn
                (setq buffer (envoy-review file snapshot "Rewrote the line."))
                (should (buffer-live-p buffer))
                (with-current-buffer buffer
                  (should envoy-review-mode)
                  (let ((text (buffer-string)))
                    (should (string-match-p "Rewrote the line\\." text))
                    (should (string-match-p "C-c C-c" text))
                    (should (string-match-p "C-c C-k" text))
                    ;; The diff itself, not just the header.
                    (should (string-match-p "^[-+]before\\|^[-+]after" text)))
                  (should (eq (lookup-key envoy-review-mode-map (kbd "C-c C-c"))
                              #'envoy-review-accept))
                  (should (eq (lookup-key envoy-review-mode-map (kbd "C-c C-k"))
                              #'envoy-review-revert))))
            (when (buffer-live-p buffer)
              (with-current-buffer buffer (setq envoy-review--snapshot nil))
              (kill-buffer buffer))
            (when (file-exists-p snapshot) (delete-file snapshot))))
      (delete-file file))))
(ert-deftest envoy-test-review-of-deleted-file-does-not-advertise-source-navigation ()
  "A review for a deleted file does not advertise source navigation."
  (let ((file (make-temp-file "envoy-test-deleted-" nil ".txt" "before\n")))
    (unwind-protect
        (let* ((envoy-review-display 'diff)
               (snapshot (envoy-snapshot file))
               review)
          (delete-file file)
          (unwind-protect
              (progn
                (setq review (envoy-review file snapshot "deleted"))
                (should (buffer-live-p review))
                (with-current-buffer review
                  (let ((text (buffer-string)))
                    (should-not (string-match-p "C-c C-v go to source" text))
                    (should (string-match-p "C-c C-k undo it\n" text))
                    (should-not (string-match-p "C-c C-i another pass\n" text)))))
            (when (buffer-live-p review)
              (with-current-buffer review (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (file-exists-p snapshot)
              (delete-file snapshot))))
      (when (file-exists-p file)
        (delete-file file)))))
(ert-deftest envoy-test-review-defers-for-an-unsaved-source-buffer-when-file-deleted ()
  "Declining to discard source edits leaves a deleted-file review pending."
  (let ((file (make-temp-file "envoy-test-deleted-" nil ".txt" "before\n")))
    (unwind-protect
        (let* ((source (let ((find-file-hook nil)) (find-file-noselect file)))
               (snapshot (envoy-snapshot file))
               review)
          (unwind-protect
              (progn
                (with-current-buffer source
                  (goto-char (point-max))
                  (insert "local\n"))
                (delete-file file)
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (&rest _) nil)))
                  (setq review (envoy-review file snapshot "deleted")))
                (should (buffer-live-p review))
                (with-current-buffer review
                  (should envoy-review--pending))
                (should (file-exists-p snapshot))
                (with-current-buffer source
                  (should (equal (buffer-string) "before\nlocal\n"))
                  (should (equal (buffer-file-name) file))
                  (should (buffer-modified-p))))
            (when (buffer-live-p review)
              (with-current-buffer review (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (file-exists-p snapshot) (delete-file snapshot))
            (when (buffer-live-p source) (kill-buffer source))))
      (when (file-exists-p file) (delete-file file)))))
(ert-deftest envoy-test-review-rejects-an-active-canonical-alias-review ()
  "A second alias for the same file cannot replace an active review."
  (let* ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
         (alias-one (concat file "-one"))
         (alias-two (concat file "-two"))
         review-one snapshot-one snapshot-two)
    (unwind-protect
        (progn
          (make-symbolic-link file alias-one)
          (make-symbolic-link file alias-two)
          (setq snapshot-one (envoy-snapshot alias-one))
          (with-temp-file file (insert "after one\n"))
          (setq review-one (envoy-review alias-one snapshot-one "first"))
          (should (buffer-live-p review-one))
          (setq snapshot-two (envoy-snapshot alias-two))
          (with-temp-file file (insert "after two\n"))
          (should-error (envoy-review alias-two snapshot-two "second")
                        :type 'user-error)
          (should-not (file-exists-p snapshot-two))
          (with-current-buffer review-one
            (should (equal envoy-review--file alias-one))
            (should (equal envoy-review--snapshot snapshot-one))))
      (when (buffer-live-p review-one)
        (with-current-buffer review-one (setq envoy-review--snapshot nil))
        (kill-buffer review-one))
      (dolist (snapshot (list snapshot-one snapshot-two))
        (when (and snapshot (file-exists-p snapshot))
          (delete-file snapshot)))
      (dolist (alias (list alias-one alias-two))
        (when (file-exists-p alias) (delete-file alias)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest envoy-test-review-rejects-active-review-before-adoption ()
  "A second result cannot replace the source buffer under review."
  (let* ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
         (alias-one (concat file "-one"))
         (alias-two (concat file "-two"))
         (source (let ((find-file-hook nil)) (find-file-noselect file)))
         review-one snapshot-one snapshot-two)
    (unwind-protect
        (progn
          (make-symbolic-link file alias-one)
          (make-symbolic-link file alias-two)
          (setq snapshot-one (envoy-snapshot alias-one))
          (with-temp-file file (insert "after one\n"))
          (setq review-one (envoy-review alias-one snapshot-one "first"))
          (should (buffer-live-p review-one))
          (with-current-buffer source
            (should (equal (buffer-string) "after one\n")))
          (setq snapshot-two (envoy-snapshot alias-two))
          (let ((replacement (make-temp-file "envoy-test-replacement-"
                                              nil ".txt" "after two\n")))
            (unwind-protect
                (copy-file replacement file t)
              (delete-file replacement)))
          (should-error (envoy-review alias-two snapshot-two "second")
                        :type 'user-error)
          (with-current-buffer source
            (should (equal (buffer-string) "after one\n"))
            (cl-letf (((symbol-function 'envoy-delegate)
                       (lambda (&rest _)
                         (ert-fail "Envoy started a rewrite"))))
              (let ((envoy--runs nil))
                (should-error
                 (envoy-rewrite (point-min) (line-end-position) "second")
                 :type 'user-error)))))
      (when (buffer-live-p review-one)
        (with-current-buffer review-one (setq envoy-review--snapshot nil))
        (kill-buffer review-one))
      (when (buffer-live-p source) (kill-buffer source))
      (dolist (snapshot (list snapshot-one snapshot-two))
        (when (and snapshot (file-exists-p snapshot))
          (delete-file snapshot)))
      (dolist (alias (list alias-one alias-two))
        (when (file-exists-p alias) (delete-file alias)))
      (when (file-exists-p file) (delete-file file)))))
(ert-deftest envoy-test-review-iteration-preserves-newer-queued-state ()
  "Accepting an iteration does not restore an older queued result."
  (let* ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
         (alias-one (concat file "-one"))
         (alias-two (concat file "-two"))
         (envoy-review--queue nil)
         (envoy-review--iterations nil)
         review-one review-iteration queued-review snapshot-one snapshot-two)
    (unwind-protect
        (progn
          (make-symbolic-link file alias-one)
          (make-symbolic-link file alias-two)
          (setq snapshot-one (envoy-snapshot alias-one))
          (with-temp-file file (insert "after one\n"))
          (setq review-one
                (envoy-review
                 alias-one snapshot-one "first"
                 (lambda (_instruction snapshot)
                   (with-temp-file file (insert "after iteration\n"))
                   (setq review-iteration
                         (envoy-review alias-one snapshot "iteration" #'ignore)))))
          (should (buffer-live-p review-one))
          (setq snapshot-two (envoy-snapshot alias-two))
          (with-temp-file file (insert "after two\n"))
          (should-not (envoy-review alias-two snapshot-two "second" #'ignore))
          (with-current-buffer review-one
            (cl-letf (((symbol-function 'quit-window) #'ignore))
              (envoy-review-iterate "more")))
          (should (buffer-live-p review-iteration))
          (with-current-buffer review-iteration
            (cl-letf (((symbol-function 'quit-window) #'ignore))
              (envoy-review-accept)))
          (setq queued-review (get-buffer (envoy--diff-buffer-name alias-two)))
          (should (buffer-live-p queued-review))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "after iteration\n"))))
      (dolist (review (list review-one review-iteration queued-review))
        (when (buffer-live-p review)
          (with-current-buffer review (setq envoy-review--snapshot nil))
          (kill-buffer review)))
      (dolist (entry envoy-review--queue)
        (dolist (snapshot (list (nth 1 entry) (nth 5 entry)))
          (when (and snapshot (file-exists-p snapshot))
            (delete-file snapshot))))
      (setq envoy-review--queue nil)
      (dolist (snapshot (list snapshot-one snapshot-two))
        (when (and snapshot (file-exists-p snapshot))
          (delete-file snapshot)))
      (dolist (alias (list alias-one alias-two))
        (when (file-exists-p alias) (delete-file alias)))
      (when (file-exists-p file) (delete-file file)))))
(ert-deftest envoy-test-review-kill-discards-queued-result ()
  "Killing an active review prevents its queued result from being replayed."
  (let* ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
         (alias-one (concat file "-one"))
         (alias-two (concat file "-two"))
         (envoy-review--queue nil)
         review-one review-two stale-review snapshot-one snapshot-two
         snapshot-three)
    (unwind-protect
        (progn
          (make-symbolic-link file alias-one)
          (make-symbolic-link file alias-two)
          (setq snapshot-one (envoy-snapshot alias-one))
          (with-temp-file file (insert "after one\n"))
          (setq review-one (envoy-review alias-one snapshot-one "first"))
          (should (buffer-live-p review-one))
          (setq snapshot-two (envoy-snapshot alias-two))
          (with-temp-file file (insert "after two\n"))
          (should-not (envoy-review alias-two snapshot-two "second" #'ignore))
          (kill-buffer review-one)
          (setq snapshot-three (envoy-snapshot alias-two))
          (with-temp-file file (insert "after three\n"))
          (setq review-two (envoy-review alias-two snapshot-three "third"))
          (should (buffer-live-p review-two))
          (with-current-buffer review-two
            (cl-letf (((symbol-function 'quit-window) #'ignore))
              (envoy-review-accept)))
          (setq stale-review (get-buffer (envoy--diff-buffer-name alias-two)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "after three\n")))
          (should-not stale-review))
      (dolist (review (list review-one review-two stale-review))
        (when (buffer-live-p review)
          (with-current-buffer review (setq envoy-review--snapshot nil))
          (kill-buffer review)))
      (dolist (entry envoy-review--queue)
        (dolist (snapshot (list (nth 1 entry) (nth 5 entry)))
          (when (and snapshot (file-exists-p snapshot))
            (delete-file snapshot))))
      (setq envoy-review--queue nil)
      (dolist (snapshot (list snapshot-one snapshot-two snapshot-three))
        (when (and snapshot (file-exists-p snapshot))
          (delete-file snapshot)))
      (dolist (alias (list alias-one alias-two))
        (when (file-exists-p alias) (delete-file alias)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest envoy-test-review-queue-survives-failed-next-review ()
  "A queued review remains queued when opening it errors."
  (let* ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
         (snapshot (envoy-snapshot file))
         (result-snapshot (envoy-snapshot file))
         (entry (list file snapshot "queued" #'ignore t result-snapshot nil nil))
         (envoy-review--queue (list entry)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "after\n"))
          (cl-letf (((symbol-function 'envoy-review)
                     (lambda (&rest _) (error "Cannot open review"))))
            (should-error (envoy--show-next-review file)))
          (should (equal envoy-review--queue (list entry)))
          (should (file-exists-p snapshot))
          (should (file-exists-p result-snapshot))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "before\n"))))
      (setq envoy-review--queue nil)
      (dolist (queued-snapshot (list snapshot result-snapshot))
        (when (and queued-snapshot (file-exists-p queued-snapshot))
          (delete-file queued-snapshot)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest envoy-test-review-defers-for-an-unsaved-source-buffer ()
  "Declining to discard source edits leaves the review pending."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n")))
    (unwind-protect
        (let* ((source (let ((find-file-hook nil)) (find-file-noselect file)))
               (snapshot (envoy-snapshot file))
               review)
          (unwind-protect
              (progn
                (with-current-buffer source
                  (goto-char (point-max))
                  (insert "local\n"))
                (with-temp-file file (insert "after\n"))
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (&rest _) nil)))
                  (setq review (envoy-review file snapshot "changed")))
                (should (buffer-live-p review))
                (with-current-buffer review
                  (should envoy-review--pending)
                  (should (string-match-p "disk edit still needs review"
                                          (buffer-string))))
                (should (file-exists-p snapshot))
                (with-current-buffer source
                  (should (equal (buffer-string) "before\nlocal\n"))
                  (should (buffer-modified-p))))
            (when (buffer-live-p review)
              (with-current-buffer review (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (file-exists-p snapshot) (delete-file snapshot))
            (when (buffer-live-p source) (kill-buffer source))))
      (delete-file file))))

(ert-deftest envoy-test-review-keeps-snapshot-when-unsaved-source-declines-without-display ()
  "Declining to discard source edits keeps a no-display review pending."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
        (envoy-review-display 'none))
    (unwind-protect
        (let* ((source (let ((find-file-hook nil)) (find-file-noselect file)))
               (snapshot (envoy-snapshot file))
               review)
          (unwind-protect
              (progn
                (with-current-buffer source
                  (goto-char (point-max))
                  (insert "local\n"))
                (with-temp-file file (insert "after\n"))
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (&rest _) nil)))
                  (setq review (envoy-review file snapshot "changed")))
                (should (buffer-live-p review))
                (with-current-buffer review
                  (should envoy-review--pending))
                (should (file-exists-p snapshot))
                (with-current-buffer source
                  (should (equal (buffer-string) "before\nlocal\n"))
                  (should (buffer-modified-p))))
            (when (buffer-live-p review)
              (with-current-buffer review (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (file-exists-p snapshot) (delete-file snapshot))
            (when (buffer-live-p source) (kill-buffer source))))
      (delete-file file))))
(ert-deftest envoy-test-revert-keeps-unsaved-source-edits ()
  "Reverting a pending review does not discard source-buffer edits."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n")))
    (unwind-protect
        (let* ((source (let ((find-file-hook nil)) (find-file-noselect file)))
               (snapshot (envoy-snapshot file))
               review)
          (unwind-protect
              (progn
                (with-current-buffer source
                  (goto-char (point-max))
                  (insert "local\n"))
                (with-temp-file file (insert "after\n"))
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (&rest _) nil)))
                  (setq review (envoy-review file snapshot "changed")))
                (should (buffer-live-p review))
                (with-current-buffer review
                  (cl-letf (((symbol-function 'y-or-n-p)
                             (lambda (&rest _) nil))
                            ((symbol-function 'quit-window) #'ignore)
                            ((symbol-function 'kill-buffer) #'ignore))
                    (envoy-review-revert)))
                (with-temp-buffer
                  (insert-file-contents file)
                  (should (equal (buffer-string) "before\n")))
                (with-current-buffer source
                  (should (equal (buffer-string) "before\nlocal\n"))
                  (should (buffer-modified-p))))
            (when (buffer-live-p review)
              (with-current-buffer review (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (file-exists-p snapshot) (delete-file snapshot))
            (when (buffer-live-p source) (kill-buffer source))))
      (delete-file file))))
(ert-deftest envoy-test-delegate-reports-pending-review-after-adoption-decline ()
  "Declining unsaved source changes still exposes the disk edit for review."
  (let* ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
         (source (let ((find-file-hook nil)) (find-file-noselect file)))
         (snapshot (envoy-snapshot file))
         messages)
    (unwind-protect
        (progn
          (with-current-buffer source
            (goto-char (point-max))
            (insert "local\n"))
          (with-temp-file file (insert "after\n"))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _) nil))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages)))
                    ((symbol-function 'envoy-run)
                     (lambda (_prompt callback &rest _keys)
                       (funcall callback
                                (envoy--result-from
                                 0 "{\"type\":\"result\",\"is_error\":false,\"result\":\"done\"}" ""))
                       'fake-process)))
            (envoy-delegate "rewrite" file :snapshot snapshot :buffer source))
          (let ((review (get-buffer (envoy--diff-buffer-name file))))
            (should (buffer-live-p review))
            (with-current-buffer review
              (should (string-match-p "after" (buffer-string)))))
          (should (string-match-p "still needs review"
                                  (mapconcat #'identity messages "\n"))))
      (setq envoy--runs nil)
      (let ((review (get-buffer (envoy--diff-buffer-name file))))
        (when (buffer-live-p review)
          (with-current-buffer review (setq envoy-review--snapshot nil))
          (kill-buffer review)))
      (when (and snapshot (file-exists-p snapshot))
        (delete-file snapshot))
      (when (buffer-live-p source) (kill-buffer source))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest envoy-test-pending-review-of-new-file-uses-empty-diff-input ()
  "A pending review of a newly created file still opens its diff."
  (let* ((file (make-temp-file "envoy-test-new-" nil ".txt"))
         (source (progn
                   (delete-file file)
                   (let ((find-file-hook nil)) (find-file-noselect file))))
         (snapshot (envoy-snapshot file))
         review)
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "local\n"))
          (with-temp-file file (insert "after\n"))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _) nil)))
            (setq review (envoy-review file snapshot "created")))
          (should (buffer-live-p review))
          (with-current-buffer review
            (should envoy-review--pending)
            (should-not envoy-review--snapshot)
            (should (string-match-p "after" (buffer-string))))
          (with-current-buffer source
            (should (equal (buffer-string) "local\n"))
            (should (buffer-modified-p))))
      (let ((review (get-buffer (envoy--diff-buffer-name file))))
        (when (buffer-live-p review)
          (with-current-buffer review (setq envoy-review--snapshot nil))
          (kill-buffer review)))
      (when (buffer-live-p source) (kill-buffer source))
      (when (file-exists-p file) (delete-file file)))))
(ert-deftest envoy-test-pending-new-file-review-does-not-advertise-undo ()
  "A pending review with no snapshot does not advertise undo."
  (let* ((file (make-temp-file "envoy-test-new-header-" nil ".txt"))
         (source (progn
                   (delete-file file)
                   (let ((find-file-hook nil)) (find-file-noselect file))))
         review)
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "local\n"))
          (with-temp-file file
            (insert "created\n"))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _) nil)))
            (setq review (envoy-review file nil "created")))
          (should (buffer-live-p review))
          (with-current-buffer review
            (should envoy-review--pending)
            (should-not envoy-review--snapshot)
            (should-not (string-match-p "C-c C-k undo it" (buffer-string)))))
      (when (buffer-live-p review)
        (with-current-buffer review
          (setq envoy-review--snapshot nil))
        (kill-buffer review))
      (when (buffer-live-p source)
        (kill-buffer source))
      (when (file-exists-p file)
        (delete-file file)))))
(ert-deftest envoy-test-revert-removes-new-file-and-keeps-local-source-edits ()
  "Reverting a pending new-file review removes disk output and keeps local edits."
  (let* ((file (make-temp-file "envoy-test-new-revert-" nil ".txt"))
         (source (progn
                   (delete-file file)
                   (let ((find-file-hook nil)) (find-file-noselect file))))
         review)
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert "local\n"))
          (with-temp-file file
            (insert "created\n"))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _) nil)))
            (setq review (envoy-review file nil "created")))
          (should (buffer-live-p review))
          (with-current-buffer review
            (cl-letf (((symbol-function 'quit-window) #'ignore)
                      ((symbol-function 'kill-buffer) #'ignore))
              (envoy-review-revert)))
          (should-not (file-exists-p file))
          (with-current-buffer source
            (should (equal (buffer-string) "local\n"))
            (should (buffer-modified-p))))
      (when (buffer-live-p review)
        (with-current-buffer review
          (setq envoy-review--snapshot nil))
        (kill-buffer review))
      (when (buffer-live-p source)
        (kill-buffer source))
      (when (file-exists-p file)
        (delete-file file)))))
(ert-deftest envoy-test-show-review-preserves-nil-snapshot-for-new-file ()
  "A new file's temporary diff input is not used as its restore snapshot."
  (let ((file (make-temp-file "envoy-test-new-" nil ".txt"))
        review)
    (unwind-protect
        (progn
          (delete-file file)
          (with-temp-file file
            (insert "created\n"))
          (setq review (envoy--show-review file nil "created" nil nil))
          (should (buffer-live-p review))
          (with-current-buffer review
            (should-not envoy-review--snapshot)
            (should (string-match-p "created" (buffer-string)))))
      (when (buffer-live-p review)
        (with-current-buffer review
          (setq envoy-review--snapshot nil))
        (kill-buffer review))
      (when (file-exists-p file)
        (delete-file file)))))
(ert-deftest envoy-test-accepting-a-deleted-file-does-not-leave-a-visiting-buffer ()
  "Accepting a deletion does not leave a stale source buffer."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n")))
    (unwind-protect
        (let* ((source (let ((find-file-hook nil)) (find-file-noselect file)))
               (snapshot (envoy-snapshot file))
               review)
          (unwind-protect
              (progn
                (delete-file file)
                (setq review (envoy-review file snapshot "deleted"))
                (should (buffer-live-p review))
                (with-current-buffer review
                  (cl-letf (((symbol-function 'quit-window) #'ignore)
                            ((symbol-function 'kill-buffer) #'ignore))
                    (envoy-review-accept)))
                (should-not (find-buffer-visiting file))
                (should-not (file-exists-p file)))
            (when (buffer-live-p review)
              (with-current-buffer review
                (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (buffer-live-p source) (kill-buffer source))
            (when (and snapshot (file-exists-p snapshot))
              (delete-file snapshot))))
      (when (file-exists-p file) (delete-file file)))))
(ert-deftest envoy-test-revert-reattaches-a-deleted-source-buffer ()
  "Reverting a deletion reattaches the source buffer to the restored file."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n")))
    (unwind-protect
        (let* ((source (let ((find-file-hook nil)) (find-file-noselect file)))
               (snapshot (envoy-snapshot file))
               review)
          (unwind-protect
              (progn
                (delete-file file)
                (setq review (envoy-review file snapshot "deleted"))
                (with-current-buffer source
                  (should-not (buffer-file-name)))
                (should (buffer-live-p review))
                (with-current-buffer review
                  (cl-letf (((symbol-function 'quit-window) #'ignore)
                            ((symbol-function 'kill-buffer) #'ignore))
                    (envoy-review-revert)))
                (should (eq (find-buffer-visiting file) source))
                (with-current-buffer source
                  (should (equal (buffer-file-name) file))
                  (should (equal (buffer-string) "before\n"))
                  (should-not (buffer-modified-p))))
            (when (buffer-live-p review)
              (with-current-buffer review (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (buffer-live-p source) (kill-buffer source))))
      (when (file-exists-p file) (delete-file file)))))
(ert-deftest envoy-test-revert-preserves-edits-to-detached-source-buffer ()
  "Reverting a deletion does not discard edits made after detachment."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n")))
    (unwind-protect
        (let* ((source (let ((find-file-hook nil)) (find-file-noselect file)))
               (snapshot (envoy-snapshot file))
               review
               prompted)
          (unwind-protect
              (progn
                (delete-file file)
                (setq review (envoy-review file snapshot "deleted"))
                (with-current-buffer source
                  (should-not (buffer-file-name))
                  (goto-char (point-max))
                  (insert "local\n")
                  (should (buffer-modified-p)))
                (with-current-buffer review
                  (cl-letf (((symbol-function 'y-or-n-p)
                             (lambda (&rest _)
                               (setq prompted t)
                               nil))
                            ((symbol-function 'quit-window) #'ignore)
                            ((symbol-function 'kill-buffer) #'ignore))
                    (envoy-review-revert)))
                (should prompted)
                (with-current-buffer source
                  (should (equal (buffer-file-name) file))
                  (should (equal (buffer-string) "before\nlocal\n"))
                  (should (buffer-modified-p))))
            (when (buffer-live-p review)
              (with-current-buffer review (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (file-exists-p snapshot) (delete-file snapshot))
            (when (buffer-live-p source) (kill-buffer source))))
      (when (file-exists-p file) (delete-file file)))))
(ert-deftest envoy-test-revert-does-not-reuse-a-reassigned-source-buffer ()
  "Reverting leaves a source buffer reassigned to another file untouched."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
        (other-file (make-temp-file "envoy-test-other-" nil ".txt" "other\n")))
    (unwind-protect
        (let* ((source (let ((find-file-hook nil)) (find-file-noselect file)))
               (snapshot (envoy-snapshot file))
               review)
          (unwind-protect
              (progn
                (delete-file file)
                (setq review (envoy-review file snapshot "deleted"))
                (with-current-buffer source
                  (set-visited-file-name other-file t)
                  (erase-buffer)
                  (insert "unrelated\n")
                  (set-buffer-modified-p t))
                (with-current-buffer review
                  (cl-letf (((symbol-function 'quit-window) #'ignore)
                            ((symbol-function 'kill-buffer) #'ignore)
                            ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                    (envoy-review-revert)))
                (with-current-buffer source
                  (should (equal (buffer-file-name) other-file))
                  (should (equal (buffer-string) "unrelated\n"))
                  (should (buffer-modified-p)))
                (with-temp-buffer
                  (insert-file-contents file)
                  (should (equal (buffer-string) "before\n"))))
            (when (buffer-live-p review)
              (with-current-buffer review (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (file-exists-p snapshot) (delete-file snapshot))
            (when (buffer-live-p source) (kill-buffer source))))
      (when (file-exists-p file) (delete-file file))
      (when (file-exists-p other-file) (delete-file other-file)))))
(ert-deftest envoy-test-accept-keeps-the-change ()
  "Accepting leaves the file as the agent left it, and drops the copy."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n")))
    (unwind-protect
        (let ((snapshot (envoy-snapshot file))
              (buffer (get-buffer-create "*envoy-test-review*")))
          (with-temp-file file (insert "after\n"))
          (unwind-protect
              (with-current-buffer buffer
                (setq envoy-review--file file
                      envoy-review--snapshot snapshot)
                (cl-letf (((symbol-function 'quit-window) #'ignore)
                          ((symbol-function 'kill-buffer) #'ignore))
                  (envoy-review-accept))
                (with-temp-buffer
                  (insert-file-contents file)
                  (should (equal (buffer-string) "after\n")))
                (should-not (file-exists-p snapshot)))
            (when (buffer-live-p buffer) (kill-buffer buffer))))
      (delete-file file))))

(ert-deftest envoy-test-accepting-a-pending-review-adopts-source-buffer ()
  "Accepting a pending review adopts the disk edit into the source buffer."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n")))
    (unwind-protect
        (let* ((source (let ((find-file-hook nil)) (find-file-noselect file)))
               (snapshot (envoy-snapshot file))
               review)
          (unwind-protect
              (progn
                (with-current-buffer source
                  (goto-char (point-max))
                  (insert "local\n"))
                (with-temp-file file (insert "after\n"))
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (&rest _) nil)))
                  (setq review (envoy-review file snapshot "changed")))
                (should (buffer-live-p review))
                (with-current-buffer review
                  (cl-letf (((symbol-function 'y-or-n-p)
                             (lambda (&rest _) t))
                            ((symbol-function 'quit-window) #'ignore)
                            ((symbol-function 'kill-buffer) #'ignore))
                    (envoy-review-accept)))
                (with-current-buffer source
                  (should (equal (buffer-string) "after\n"))
                  (should-not (buffer-modified-p))))
            (when (buffer-live-p review)
              (with-current-buffer review (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (file-exists-p snapshot) (delete-file snapshot))
            (when (buffer-live-p source) (kill-buffer source))))
      (when (file-exists-p file) (delete-file file)))))
(ert-deftest envoy-test-accepting-a-pending-review-keeps-review-when-source-edits-remain ()
  "Declining to discard source edits keeps an accepting review open."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n")))
    (unwind-protect
        (let* ((source (let ((find-file-hook nil)) (find-file-noselect file)))
               (snapshot (envoy-snapshot file))
               review)
          (unwind-protect
              (progn
                (with-current-buffer source
                  (goto-char (point-max))
                  (insert "local\n"))
                (with-temp-file file (insert "after\n"))
                (cl-letf (((symbol-function 'y-or-n-p)
                           (lambda (&rest _) nil)))
                  (setq review (envoy-review file snapshot "changed")))
                (should (buffer-live-p review))
                (with-current-buffer review
                  (cl-letf (((symbol-function 'y-or-n-p)
                             (lambda (&rest _) nil))
                            ((symbol-function 'quit-window) #'ignore)
                            ((symbol-function 'kill-buffer) #'ignore))
                    (should-error (envoy-review-accept) :type 'user-error)))
                (should (buffer-live-p review))
                (should (file-exists-p snapshot))
                (with-current-buffer source
                  (should (equal (buffer-string) "before\nlocal\n"))
                  (should (buffer-modified-p))))
            (when (buffer-live-p review)
              (with-current-buffer review (setq envoy-review--snapshot nil))
              (kill-buffer review))
            (when (file-exists-p snapshot) (delete-file snapshot))
            (when (buffer-live-p source) (kill-buffer source))))
      (when (file-exists-p file) (delete-file file)))))
(ert-deftest envoy-test-review-commands-refuse-a-foreign-buffer ()
  "The review commands do nothing outside a review buffer."
  (with-temp-buffer
    (should-error (envoy-review-accept) :type 'user-error)
    (should-error (envoy-review-revert) :type 'user-error)))

(ert-deftest envoy-test-adopt-preserves-point-and-markers ()
  "Taking up the agent's edit leaves point, markers and overlays in place.

`revert-buffer' would put point back at the top of the buffer and drop
the markers and overlays with it, which is why the new text is merged in
with `replace-buffer-contents' instead.

Point is checked by what line it is on rather than by its offset: the
rewritten line is longer than the one it replaces, so an offset below it
is supposed to move.  Staying on \"third line\" is the property that
matters -- the line under point before the run is the line under point
after it."
  (let ((file (make-temp-file "envoy-test-" nil ".txt"
                              "first line\nsecond line\nthird line\n")))
    (unwind-protect
        (let ((buffer (let ((find-file-hook nil)) (find-file-noselect file))))
          (unwind-protect
              (with-current-buffer buffer
                (goto-char (point-min))
                (forward-line 2)
                (let ((column 6)
                      ;; Both of these sit in the first line, which the
                      ;; agent does not touch, so they are expected at
                      ;; exactly the offsets they started at.
                      (marker (copy-marker 3))
                      (overlay (make-overlay 1 6)))
                  (forward-char column)
                  ;; The agent rewrites the middle line only.
                  (with-temp-file file
                    (insert "first line\nSECOND LINE CHANGED\nthird line\n"))
                  (envoy--adopt-into-buffer file)
                  (should (string-match-p "SECOND LINE CHANGED" (buffer-string)))
                  (should (equal (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))
                                 "third line"))
                  (should (equal (current-column) column))
                  (should (equal (marker-position marker) 3))
                  (should (overlay-buffer overlay))
                  (should (equal (overlay-start overlay) 1))
                  (should (equal (overlay-end overlay) 6))
                  ;; And the buffer is not left looking unsaved.
                  (should-not (buffer-modified-p))))
            (kill-buffer buffer)))
      (delete-file file))))

;;; The rewrite brief
(ert-deftest envoy-test-rewrite-prompt-names-the-file-and-the-lines ()
  "The rewrite brief names the file, range and selected context."
  (let ((file (make-temp-file "envoy-test-" nil ".txt"
                              "before one\nselected line\nafter one\n")))
    (unwind-protect
        (let ((prompt (envoy--rewrite-prompt file 2 2 "Say it plainly.")))
          (should (equal (car (split-string prompt "\n"))
                         (format "Rewrite lines 2 to 2 of %s." file)))
          (should (string-match-p "Say it plainly\\." prompt))
          (should (string-match-p "in place" prompt))
          (should (string-match-p "selected line" prompt)))
      (delete-file file))))
(ert-deftest envoy-test-named-instruction-expands ()
  "The rewrite instruction is the nonblank text typed by the user."
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _) "make it rhyme")))
    (should (equal (envoy--read-instruction "? ") "make it rhyme")))
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _) "   ")))
    (should-error (envoy--read-instruction "? ") :type 'user-error)))

(ert-deftest envoy-test-direction-trims-and-defaults-blank-input ()
  "A direction is trimmed, and a blank answer uses the configured default."
  (let ((answers '("  Be careful  " " \t ")))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) (pop answers))))
      (should (equal (envoy--read-direction "Direction: ") "Be careful"))
      (should (equal (envoy--read-direction "Direction: ")
                     envoy-default-direction)))))


(defun envoy-test--rewrite-prompt-for (contents beg end)
  "Return the brief `envoy-rewrite' builds for BEG to END over CONTENTS.
The agent is never started: `envoy-delegate' is replaced by something
that keeps the brief it was handed."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" contents))
        prompt)
    (unwind-protect
        (let ((buffer (let ((find-file-hook nil)) (find-file-noselect file))))
          (unwind-protect
              (with-current-buffer buffer
                (cl-letf (((symbol-function 'envoy-delegate)
                           (lambda (p &rest _) (setq prompt p) nil)))
                  (envoy-rewrite beg end "Say it plainly."))
                prompt)
            (kill-buffer buffer)))
      (delete-file file))))

(ert-deftest envoy-test-region-ending-at-a-line-start ()
  "A region ending at the start of a line stops at the line before it.

Selecting two lines with \\[next-line] leaves point at the start of the
third, because what the region covers is the newline that ends the second
line, not the third line itself.  Reporting the third line as part of the
range would invite the agent to rewrite a line nobody selected."
  (let ((contents "one\ntwo\nthree\nfour\n"))
    ;; Lines one and two, point left at the start of line three.
    (should (string-match-p
             "lines 1 to 2 of"
             (envoy-test--rewrite-prompt-for contents 1 9)))
    ;; A region that does end inside a line covers that line.
    (should (string-match-p
             "lines 1 to 3 of"
             (envoy-test--rewrite-prompt-for contents 1 11)))
    ;; And an empty region at the start of a line is that line, not the
    ;; one before it -- the guard is on a region with something in it.
    (should (string-match-p
             "lines 2 to 2 of"
             (envoy-test--rewrite-prompt-for contents 5 5)))))
(ert-deftest envoy-test-rewrite-quotes-only-the-selected-lines ()
  "The selected lines are marked separately from read-only context."
  (let* ((file (make-temp-file "envoy-test-" nil ".txt"
                               "one\ntwo\nthree\nfour\n"))
         (envoy-rewrite-context-lines 1))
    (unwind-protect
        (let* ((prompt (envoy--rewrite-prompt file 2 2 "Say it plainly."))
               (start (string-match "<<< SELECTION TO REWRITE STARTS >>>" prompt))
               (end (string-match "<<< SELECTION TO REWRITE ENDS >>>" prompt start)))
          (should start)
          (should end)
          (should (equal (substring prompt (+ start 36) end) "two\n"))
          (should (string-match-p "one" (substring prompt 0 start)))
          (should (string-match-p "three" (substring prompt (+ end 33)))))
      (delete-file file))))



(ert-deftest envoy-test-rewrite-needs-a-region ()
  "With nothing selected the command says so rather than guessing."
  (with-temp-buffer
    (insert "some text\n")
    (cl-letf (((symbol-function 'use-region-p) #'ignore))
      (should-error (call-interactively #'envoy-rewrite) :type 'user-error))))
(ert-deftest envoy-test-lines-are-read-from-disk ()
  "The selected excerpt is read from the file as it stands now."
  (let ((envoy-rewrite-context-lines 0)
        (file (make-temp-file "envoy-test-" nil ".txt"
                              "one\ntwo\nthree\nfour\n")))
    (unwind-protect
        (progn
          (should (equal (envoy--rewrite-excerpt file 2 3)
                         '("" "two\nthree\n" "")))
          (with-temp-file file (insert "new-one\nnew-two\nnew-three\nnew-four\n"))
          (should (equal (envoy--rewrite-excerpt file 2 3)
                         '("" "new-two\nnew-three\n" ""))))
      (delete-file file))))


;;; Org headings

(defmacro envoy-test--with-org (text &rest body)
  "Evaluate BODY in an org buffer holding TEXT, with point at its start."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-mode-hook nil)
           (org-inhibit-startup t))
       (org-mode)
       (insert ,text)
       (goto-char (point-min))
       ,@body)))
(defmacro envoy-test--with-org-file (text &rest body)
  "Evaluate BODY in a temporary file-backed org buffer holding TEXT."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "envoy-test-org-" t))
          (file (expand-file-name "tasks.org" directory))
          (envoy-org-spool-directory
           (expand-file-name "spool" directory))
          (org-id-locations-file (expand-file-name ".org-id-locations"
                                                   directory))
          (org-id-locations nil)
          (org-agenda-files nil)
          buffer)
     (unwind-protect
         (progn
           (with-temp-file file (insert ,text))
           (setq buffer (find-file-noselect file))
           (with-current-buffer buffer
             (cl-letf (((symbol-function 'envoy--read-direction)
                        (lambda (_prompt) envoy-default-direction)))
               (goto-char (point-min))
               (re-search-forward "^\\*+ ")
               (org-back-to-heading t)
               ,@body)))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-directory directory t))))


(ert-deftest envoy-test-agent-report-does-not-create-an-org-link ()
  "Agent text stays plain text when it is filed under a heading."
  (envoy-test--with-org "* TODO The task\n"
    (org-back-to-heading t)
    (envoy-org--record
     (point-marker) (current-buffer)
     "A report with [[elisp:(error \"pwned\")][run]].")
    (should-not
     (org-element-map (org-element-parse-buffer) 'link #'identity))))

(ert-deftest envoy-test-multiline-agent-report-does-not-create-an-org-link ()
  "A multiline agent link stays plain text when filed under a heading."
  (envoy-test--with-org "* TODO The task\n"
    (org-back-to-heading t)
    (envoy-org--record
     (point-marker) (current-buffer)
     "safe\n[[elisp:(setq envoy-test-pwned t)\n][run]]")
    (should-not
     (org-element-map (org-element-parse-buffer) 'link #'identity))))


(ert-deftest envoy-test-agent-report-macro-does-not-export-active-html ()
  "A report macro cannot export an active HTML script."
  (envoy-test--with-org "* TODO The task\n"
    (org-back-to-heading t)
    (envoy-org--record
     (point-marker) (current-buffer)
     "{{{time(@@html:<script>alert(1)</script>@@)}}}")
    (let ((html (org-export-as 'html nil nil t)))
      (should-not (string-match-p "<script>alert(1)</script>" html)))))

(ert-deftest envoy-test-agent-report-unknown-macros-stay-literal-after-expansion ()
  "Unknown macros stay literal when Org expands the report."
  (envoy-test--with-org "* TODO The task\n"
    (org-back-to-heading t)
    (envoy-org--record
     (point-marker) (current-buffer)
     "Inline {{{unknown-inline}}} and\n{{{unknown-standalone}}}")
    (let ((expansion-error
           (condition-case err
               (progn (org-macro-replace-all nil) nil)
             (error err))))
      (should-not expansion-error)
      (let ((text (buffer-string)))
        (should (string-match-p "{{{unknown-inline}}}" text))
        (should (string-match-p "{{{unknown-standalone}}}" text))))))
(ert-deftest envoy-test-tmux-command-keeps-claude-brief-out-of-argv ()
  "A Claude command does not put the brief contents in its arguments."
  (let* ((prompt-file (make-temp-file "envoy-test-prompt-" nil ".txt"
                                      "PRIVATE BRIEF"))
         (argv-file (make-temp-file "envoy-test-argv-"))
         (program (make-temp-file "envoy-test-program-" nil ".sh")))
    (unwind-protect
        (progn
          (with-temp-file program
            (insert "#!/bin/sh\n"
                    (format "printf '%%s\\n' \"$@\" > %s\n"
                            (shell-quote-argument argv-file))))
          (set-file-modes program #o755)
          (let* ((claude (copy-sequence (envoy-tmux--provider 'claude)))
                 (envoy-tmux-providers
                  (list (cons 'claude (plist-put claude :program program))))
                 (command (envoy-tmux-command prompt-file 'claude)))
            (should (= 0 (call-process "sh" nil nil nil "-c" command)))
            (with-temp-buffer
              (insert-file-contents argv-file)
              (let ((argv (split-string (buffer-string) "\n" t)))
                (should-not (member "PRIVATE BRIEF" argv))))))
      (dolist (file (list prompt-file argv-file program))
        (when (file-exists-p file)
          (delete-file file))))))


(ert-deftest envoy-test-org-heading-does-not-file-result-under-replaced-heading ()
  "A result is not filed when the delegated heading changes before completion."
  (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "test agent"
                                     :args nil :edit-args nil
                                     :deny-arg nil :model-arg "--model")))
          callback
          messages)
      (cl-letf (((symbol-function 'envoy-run)
                 (lambda (_prompt cb &rest _keys)
                   (setq callback cb)
                   'fake-process))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (envoy-org-heading)
        (goto-char (point-min))
        (re-search-forward "^\\* DOING The task$")
        (replace-match "* TODO Replacement")
        (funcall callback
                 (envoy--make-result
                  :ok t :text "ENVOY_STATUS: complete\nDone." :status 0))
        (should (equal (org-get-todo-state) "TODO"))
        (should-not (string-match-p ":ENVOY:" (buffer-string)))
        (should (seq-some (lambda (message)
                            (string-match-p "was not filed" message))
                          messages))))))

(ert-deftest envoy-test-org-heading-does-not-file-result-under-same-title-replacement ()
  "A same-title replacement cannot receive the old run's result."
  (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "test agent"
                                     :args nil :edit-args nil
                                     :deny-arg nil :model-arg "--model")))
          callback
          messages)
      (cl-letf (((symbol-function 'envoy-run)
                 (lambda (_prompt cb &rest _keys)
                   (setq callback cb)
                   'fake-process))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (envoy-org-heading)
        (goto-char (point-min))
        (re-search-forward "^\\* DOING The task$")
        (let ((begin (line-beginning-position)))
          (delete-region begin (point-max))
          (insert "* TODO The task\n"))
        (funcall callback
                 (envoy--make-result
                  :ok t :text "ENVOY_STATUS: complete\nDone." :status 0))
        (should (equal (org-get-todo-state) "TODO"))
        (should-not (string-match-p ":ENVOY:" (buffer-string)))
        (should (seq-some (lambda (message)
                            (string-match-p "was not filed" message))
                          messages))))))

(ert-deftest envoy-org-heading-id-validation-does-not-parse-buffer ()
  "Heading ID validation does not reparse the whole buffer."
  (envoy-test--with-org-file "* TODO Same\n"
    (let ((identity (envoy-org--heading-identity))
          (marker (point-marker))
          (buffer (current-buffer)))
      (cl-letf (((symbol-function 'org-element-parse-buffer)
                 (lambda (&rest _args)
                   (error "Unexpected full-buffer parse"))))
        (should (envoy-org--heading-id-matches-p marker buffer identity))))))
(ert-deftest envoy-org-drawer-bounds-does-not-parse-body ()
  "Drawer lookup does not parse the whole heading body."
  (envoy-test--with-org "* TODO The task\n:ENVOY:\nA report.\n:END:\n"
    (org-back-to-heading t)
    (cl-letf (((symbol-function 'org-element-parse-buffer)
               (lambda (&rest _args)
                 (error "Unexpected full-body parse"))))
      (should (envoy-org--drawer-bounds "ENVOY")))))

(ert-deftest envoy-org-record-does-not-delete-child-after-unterminated-drawer ()
  "An unterminated report drawer cannot consume a child subtree."
  (envoy-test--with-org
      "* TODO Parent\n:ENVOY:\nOld report.\n** Child\nChild body.\n:END:\n"
    (org-back-to-heading t)
    (envoy-org--record (point-marker) (current-buffer) "New report.")
    (should (string-match-p "^\\*\\* Child$" (buffer-string)))
    (should (string-match-p "Child body\\." (buffer-string)))))

(ert-deftest envoy-org-rename-test-rejects-duplicate-id-replacement ()
  "A duplicate-ID replacement cannot receive the old run's result."
  (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO Same\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "test agent"
                                     :args nil :edit-args nil
                                     :deny-arg nil :model-arg "--model")))
          callback)
      (cl-letf (((symbol-function 'envoy-run)
                 (lambda (_prompt cb &rest _keys)
                   (setq callback cb)
                   'fake-process))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil)))
        (envoy-org-heading)
        (let ((id (org-entry-get nil "ID")))
          (goto-char (point-min))
          (re-search-forward "^\\* DOING Same$")
          (let ((begin (line-beginning-position)))
            (delete-region begin (point-max))
            (insert (format "* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n"
                            id id))))
        (funcall callback
                 (envoy--make-result
                  :ok t :text "ENVOY_STATUS: complete\nDone." :status 0))
        (should (equal (org-get-todo-state) "TODO"))
        (should-not (string-match-p ":ENVOY:" (buffer-string)))))))
(ert-deftest envoy-org-duplicate-id-after-drawer-marker-does-not-file-result ()
  "A duplicate ID after a drawer marker cannot receive the old run's result."
  (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO Same\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "test agent"
                                     :args nil :edit-args nil
                                     :deny-arg nil :model-arg "--model")))
          callback
          messages)
      (cl-letf (((symbol-function 'envoy-run)
                 (lambda (_prompt cb &rest _keys)
                   (setq callback cb)
                   'fake-process))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (envoy-org-heading)
        (let ((id (org-entry-get nil "ID")))
          (goto-char (point-min))
          (re-search-forward "^\\* DOING Same$")
          (let ((begin (line-beginning-position)))
            (delete-region begin (point-max))
            (insert (format "* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n:ENVOY:\n#+begin_foo\n:END:\n* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n#+end_foo\n"
                            id id))))
        (funcall callback
                 (envoy--make-result
                  :ok t :text "ENVOY_STATUS: complete\nDone." :status 0))
        (should-not (string-match-p "Done\\." (buffer-string)))
        (should (seq-some (lambda (message)
                            (string-match-p "was not filed" message))
                          messages))))))
(ert-deftest envoy-org-uppercase-source-block-does-not-count-duplicate-id ()
  "A duplicate ID inside an uppercase source block is ignored."
  (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO Same\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "test agent"
                                     :args nil :edit-args nil
                                     :deny-arg nil :model-arg "--model")))
          callback)
      (cl-letf (((symbol-function 'envoy-run)
                 (lambda (_prompt cb &rest _keys)
                   (setq callback cb)
                   'fake-process))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil)))
        (envoy-org-heading)
        (let ((id (org-entry-get nil "ID")))
          (goto-char (point-min))
          (re-search-forward "^\\* DOING Same$")
          (let ((begin (line-beginning-position)))
            (delete-region begin (point-max))
            (insert (format "* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n#+BEGIN_SRC org\n* TODO Fake\n:PROPERTIES:\n:ID: %s\n:END:\n#+END_SRC\n"
                            id id))))
        (funcall callback
                 (envoy--make-result
                  :ok t :text "ENVOY_STATUS: complete\nDone." :status 0))
        (should (string-match-p ":ENVOY:" (buffer-string)))))))
(ert-deftest envoy-org-indented-top-level-block-markers-do-not-hide-duplicate-id ()
  "An indented top-level block marker does not hide a duplicate ID heading."
  (envoy-test--with-org-file "* TODO Same\n"
    (let* ((identity (envoy-org--heading-identity))
           (marker (point-marker))
           (buffer (current-buffer))
           (id (nth 0 identity)))
      (goto-char (point-max))
      (insert (format "  #+begin_src text\n* TODO Fake\n:PROPERTIES:\n:ID: %s\n:END:\n  #+end_src\n"
                      id))
      (should-not (envoy-org--heading-id-matches-p marker buffer identity)))))

(ert-deftest envoy-org-indented-drawer-marker-does-not-hide-duplicate-id ()
  "An indented non-drawer marker does not hide a duplicate ID heading."
  (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO Same\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "test agent"
                                     :args nil :edit-args nil
                                     :deny-arg nil :model-arg "--model")))
          callback
          messages)
      (cl-letf (((symbol-function 'envoy-run)
                 (lambda (_prompt cb &rest _keys)
                   (setq callback cb)
                   'fake-process))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (envoy-org-heading)
        (let ((id (org-entry-get nil "ID")))
          (goto-char (point-min))
          (re-search-forward "^\\* DOING Same$")
          (let ((begin (line-beginning-position)))
            (delete-region begin (point-max))
            (insert (format "* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n  :FOO:\n* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n"
                            id id))))
        (funcall callback
                 (envoy--make-result
                  :ok t :text "ENVOY_STATUS: complete\nDone." :status 0))
        (should-not (string-match-p "Done\\." (buffer-string)))
        (should (seq-some (lambda (message)
                            (string-match-p "was not filed" message))
                          messages))))))
(ert-deftest envoy-org-unrelated-end-marker-does-not-count-duplicate-id ()
  "An unrelated block end does not expose a heading inside the source block."
  (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO Same\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "test agent"
                                     :args nil :edit-args nil
                                     :deny-arg nil :model-arg "--model")))
          callback)
      (cl-letf (((symbol-function 'envoy-run)
                 (lambda (_prompt cb &rest _keys)
                   (setq callback cb)
                   'fake-process))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil)))
        (envoy-org-heading)
        (let ((id (org-entry-get nil "ID")))
          (goto-char (point-min))
          (re-search-forward "^\\* DOING Same$")
          (let ((begin (line-beginning-position)))
            (delete-region begin (point-max))
            (insert (format "* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n#+begin_src text\n* TODO Fake1\n:PROPERTIES:\n:ID: %s\n:END:\n#+end_example\n* TODO Fake2\n:PROPERTIES:\n:ID: %s\n:END:\n#+end_src\n"
                            id id id))))
        (funcall callback
                 (envoy--make-result
                  :ok t :text "ENVOY_STATUS: complete\nDone." :status 0))
        (should (string-match-p ":ENVOY:" (buffer-string)))))))
(ert-deftest envoy-org-begin-marker-inside-block-does-not-hide-duplicate-id ()
  "A BEGIN marker inside a source block does not hide a following heading."
  (envoy-test--with-org-file "* TODO Same\n"
    (let* ((identity (envoy-org--heading-identity))
           (marker (point-marker))
           (buffer (current-buffer))
           (id (nth 0 identity)))
      (goto-char (point-max))
      (insert (format "#+BEGIN_SRC org\n#+BEGIN_SRC literal\n#+END_SRC\n* TODO Fake\n:PROPERTIES:\n:ID: %s\n:END:\n"
                      id))
      (should-not (envoy-org--heading-id-matches-p marker buffer identity)))))
(ert-deftest envoy-org-first-end-marker-closes-source-block ()
  "A same-name BEGIN marker in source text does not hide a later heading."
  (envoy-test--with-org-file "* TODO Same\n"
    (let* ((identity (envoy-org--heading-identity))
           (marker (point-marker))
           (buffer (current-buffer))
           (id (nth 0 identity)))
      (goto-char (point-max))
      (insert (format "#+BEGIN_SRC org\n#+BEGIN_SRC org\n#+END_SRC\n* TODO Fake\n:PROPERTIES:\n:ID: %s\n:END:\n#+END_SRC\n"
                      id))
      (should-not (envoy-org--heading-id-matches-p marker buffer identity)))))
(ert-deftest envoy-test-org-structural-regexps-require-blank-whitespace ()
  "Org markers accept spaces and tabs, but not letters or backslashes."
  (should (equal (envoy-org--sanitise "t:END:") ": t:END:"))
  (should (equal (envoy-org--sanitise ":END:") ",:END:"))
  (should (equal (envoy-org--sanitise "* Heading") ",* Heading"))
  (should (string-match-p envoy-org--block-regexp
                          "  #+begin_src emacs-lisp"))
  (should (string-match-p envoy-org--block-regexp
                          "\t#+begin_src emacs-lisp"))
  (should-not (string-match-p envoy-org--block-regexp
                              "t#+begin_src emacs-lisp"))
  (should-not (string-match-p envoy-org--block-regexp
                              "\\#+begin_src emacs-lisp"))
  (let ((block-with-newline "#+begin_emacs-lisp\nbody"))
    (should (string-match envoy-org--block-regexp block-with-newline))
    (should (equal (match-string-no-properties 2 block-with-newline)
                   "emacs-lisp")))
  (should (string-match-p envoy-org--drawer-regexp "  :RESULTS:"))
  (should (string-match-p envoy-org--drawer-regexp "\t:RESULTS:"))
  (should-not (string-match-p envoy-org--drawer-regexp "t:RESULTS:"))
  (should-not (string-match-p envoy-org--drawer-regexp "\\:RESULTS:")))

(ert-deftest envoy-test-org-drawer-end-requires-blank-whitespace ()
  "A drawer ends at a tab-indented :END:, not at lookalikes."
  (envoy-test--with-org
      "* TODO The task\n:RESULTS:\nt:END:\n\t:END:\n"
    (goto-char (point-min))
    (re-search-forward "^:RESULTS:$")
    (beginning-of-line)
    (let ((expected
           (save-excursion
             (forward-line 2)
             (line-beginning-position 2))))
      (should (equal (envoy-org--drawer-end (point-max)) expected))))
  (envoy-test--with-org
      "* TODO The task\n:RESULTS:\n\\:END:\n\t:END:\n"
    (goto-char (point-min))
    (re-search-forward "^:RESULTS:$")
    (beginning-of-line)
    (let ((expected
           (save-excursion
             (forward-line 2)
             (line-beginning-position 2))))
      (should (equal (envoy-org--drawer-end (point-max)) expected)))))

(ert-deftest envoy-test-datetree-headings-are-recognised ()
  "A datetree node is told apart from a heading that starts with a date.

The second half of this matters more than the first.  A heading called
\"2026-07-20 malware sweep\" is real context and has to survive being
collected, which is why the test is anchored at both ends."
  (should (envoy-org--datetree-heading-p "2026"))
  (should (envoy-org--datetree-heading-p "2026-08"))
  (should (envoy-org--datetree-heading-p "2026-08 August"))
  (should (envoy-org--datetree-heading-p "2026-W32"))
  (should (envoy-org--datetree-heading-p "2026-W32 w32"))
  (should (envoy-org--datetree-heading-p "2026-08-08"))
  (should (envoy-org--datetree-heading-p "2026-08-08 Saturday"))
  ;; A keyword in front of one is still one.
  (should (envoy-org--datetree-heading-p "DONE 2026-08-08 Saturday"))
  ;; And these are content.  A heading that merely begins with a date:
  (should-not (envoy-org--datetree-heading-p "2026-07-20 malware sweep"))
  (should-not (envoy-org--datetree-heading-p "2026 planning"))
  (should-not (envoy-org--datetree-heading-p "2026-08 retrospective"))
  ;; and one that merely ends with one, which is just as ordinary a way to
  ;; name a project heading.
  (should-not (envoy-org--datetree-heading-p "Q3 2026"))
  (should-not (envoy-org--datetree-heading-p "roadmap 2026-08"))
  (should-not (envoy-org--datetree-heading-p "kickoff 2026-08-08"))
  (should-not (envoy-org--datetree-heading-p "sprint 2026-W32"))
  (should-not (envoy-org--datetree-heading-p "Example Client"))
  (should-not (envoy-org--datetree-heading-p "")))

(ert-deftest envoy-test-ancestors-collected-outermost-first ()
  "Every ancestor comes along, outermost first."
  (envoy-test--with-org "* Example Client\n** Website\n*** TODO Write the copy\n"
    (goto-char (point-max))
    (org-back-to-heading t)
    (let ((context (envoy-org--ancestor-context)))
      (should context)
      (should (string-match-p "Example Client" context))
      (should (string-match-p "Website" context))
      ;; Outermost first, so Example Client comes before Website.
      (should (< (string-match "Example Client" context)
                 (string-match "Website" context)))
      ;; The task itself is not part of its own context.
      (should-not (string-match-p "Write the copy" context)))))

(ert-deftest envoy-test-collection-stops-below-a-datetree ()
  "Nothing above a datetree node is collected, including the node."
  (envoy-test--with-org
      "* Example Client\n** 2026-08-08 Saturday\n*** TODO Write the copy\n"
    (goto-char (point-max))
    (org-back-to-heading t)
    (should-not (envoy-org--ancestor-context))))

(ert-deftest envoy-test-ancestor-body-excludes-other-children ()
  "An ancestor contributes its own body, not its other children's."
  (envoy-test--with-org
      "* Project\nThe budget is fixed.\n** Sibling\nIrrelevant detail.\n\
** TODO The task\nDo the thing.\n"
    (goto-char (point-max))
    (org-back-to-heading t)
    (let ((context (envoy-org--ancestor-context)))
      (should (string-match-p "The budget is fixed\\." context))
      (should-not (string-match-p "Irrelevant detail" context)))))

(ert-deftest envoy-test-meta-lines-keep-properties-drop-logbook ()
  "Properties and deadlines are kept; logbook and clock lines are not."
  (envoy-test--with-org
      "* TODO The task\nDEADLINE: <2026-08-09 Sun>\n:PROPERTIES:\n\
:CLIENT: Example Client\n:END:\n:LOGBOOK:\nCLOCK: [2026-08-08 Sat 10:00]--\
[2026-08-08 Sat 11:00] =>  1:00\n:END:\nDo the thing.\n"
    (org-back-to-heading t)
    (let ((meta (mapconcat #'identity (envoy-org--meta-lines) "\n")))
      (should (string-match-p ":CLIENT: Example Client" meta))
      (should (string-match-p "DEADLINE:" meta))
      (should-not (string-match-p "CLOCK:" meta))
      (should-not (string-match-p "LOGBOOK" meta)))))

(ert-deftest envoy-test-brief-has-its-sections-in-order ()
  "The brief reads context, then where to put things, then the task."
  (envoy-test--with-org "* Example Client\nBudget is fixed.\n** TODO Write the copy\n\
Do the thing.\n"
    (goto-char (point-max))
    (org-back-to-heading t)
    (let ((prompt (envoy-org-build-prompt
                   "Write the copy" (envoy-org--subtree-text)
                   "/tmp/envoy-test-attach" nil)))
      (should (string-match-p "From an Emacs org heading: Write the copy" prompt))
      (should (string-match-p "Example Client" prompt))
      (should (string-match-p "/tmp/envoy-test-attach" prompt))
      (should (string-match-p "## The task" prompt))
      (should (< (string-match "headings above" prompt)
                 (string-match "Where to put things" prompt)))
      (should (< (string-match "Where to put things" prompt)
                 (string-match "## The task" prompt))))))

(ert-deftest envoy-test-org-brief-report-contract-follows-task-text ()
  "The org brief puts its report contract after the task text."
  (envoy-test--with-org "* TODO The task\nDo the thing.\n"
    (org-back-to-heading t)
    (let* ((prompt (envoy-org-build-prompt
                    "The task" (envoy-org--subtree-text) nil nil "brief-key"))
           (task (string-match "## The task" prompt))
           (body (string-match "Do the thing\\." prompt))
           (status (string-match "## Completion status" prompt))
           (closing (string-match "## Closing report" prompt))
           (spool (string-match "## Where to leave your report" prompt)))
      (should (string-match-p "Do the thing\\.\n\n## Completion status" prompt))
      (should (< task body status closing spool)))))

(ert-deftest envoy-test-brief-routes-authorization-gates-through-ask ()
  "A step that needs live authorization is asked in-session, never parked in a file."
  (envoy-test--with-org "* TODO The task\nDo the thing.\n"
    (org-back-to-heading t)
    (let ((prompt (envoy-org-build-prompt
                   "The task" (envoy-org--subtree-text) "/tmp/x" nil)))
      (should (string-match-p "## Steps that need the user's approval" prompt))
      (should (string-match-p "`ask` tool" prompt))
      (should (string-match-p "Never write a separate handoff" prompt))
      (should (< (string-match "Steps that need the user's approval" prompt)
                 (string-match "## The task" prompt))))))

(ert-deftest envoy-test-brief-can-omit-output-instructions ()
  "Turning off the output instructions leaves them out of the brief."
  (envoy-test--with-org "* TODO The task\nDo the thing.\n"
    (org-back-to-heading t)
    (let* ((envoy-org-output-instructions nil)
           (prompt (envoy-org-build-prompt
                    "The task" (envoy-org--subtree-text) "/tmp/x" nil)))
      (should-not (string-match-p "Where to put things" prompt))
      (should (string-match-p "## The task" prompt)))))

(ert-deftest envoy-test-subtree-text-includes-children ()
  "The task itself is sent whole, children and all."
  (envoy-test--with-org "* TODO Parent\nBody.\n** Child\nMore.\n* Next\n"
    (org-back-to-heading t)
    (let ((subtree (envoy-org--subtree-text)))
      (should (string-match-p "Parent" subtree))
      (should (string-match-p "Child" subtree))
      (should (string-match-p "More\\." subtree))
      ;; But not the heading after it.
      (should-not (string-match-p "Next" subtree)))))

(ert-deftest envoy-test-attachment-listing-creates-nothing ()
  "Listing attachments never brings a directory into being."
  (let ((directory (expand-file-name "envoy-test-no-such-attach-dir"
                                     temporary-file-directory)))
    (when (file-directory-p directory) (delete-directory directory t))
    (should-not (envoy-org--attachment-files directory))
    (should-not (file-directory-p directory))))

(ert-deftest envoy-test-ancestor-attachments-dedupe-by-directory ()
  "A directory shared by several ancestors is listed once, outermost.

Attachment directories are routinely shared through a tag, so a project
and each of its milestones resolve to the same place; listing it per
ancestor would repeat the whole file list for nothing."
  (let ((directory (make-temp-file "envoy-test-attach-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "brief.org" directory)
            (insert "brief\n"))
          (envoy-test--with-org "* Project\n** Milestone\n*** TODO The task\n"
            (goto-char (point-max))
            (org-back-to-heading t)
            (cl-letf (((symbol-function 'org-attach-dir)
                       (lambda (&rest _) directory)))
              (let ((found (envoy-org--ancestor-attachments)))
                (should (equal (length found) 1))
                (should (equal (nth 0 (car found)) "Project"))
                (should (equal (nth 1 (car found)) directory))
                (should (equal (length (nth 2 (car found))) 1)))
              ;; And the task's own directory is left out when excluded.
              (should-not (envoy-org--ancestor-attachments (list directory))))))
      (delete-directory directory t))))

(ert-deftest envoy-test-attach-directory-tags-the-heading ()
  "Creating the directory marks the heading as holding attachments.

Every org command that creates an attachment directory tags the heading
-- `org-attach-buffer', `org-attach-attach' and `org-attach-new' all do
-- and the tag is how an agenda search finds attached headings later.  A
directory the agent has written into, under a heading org considers
unattached, is the thing this prevents."
  (let ((directory (make-temp-file "envoy-test-attach-" t)))
    (unwind-protect
        (envoy-test--with-org "* TODO The task\n"
          (org-back-to-heading t)
          (cl-letf (((symbol-function 'org-attach-dir)
                     (lambda (&rest _) directory)))
            (should (equal (envoy-org--attach-directory) directory))
            (should (member "ATTACH" (org-get-tags nil t))))
          (envoy-test--with-org "* TODO The task\n"
            (org-back-to-heading t)
            (let ((org-attach-auto-tag "FILES"))
              (cl-letf (((symbol-function 'org-attach-dir)
                         (lambda (&rest _) directory)))
                (should (equal (envoy-org--attach-directory) directory))
                (should (member "FILES" (org-get-tags nil t)))
                (should-not (member "ATTACH" (org-get-tags nil t))))))
          ;; And a user who has turned the tag off keeps it off.
          (envoy-test--with-org "* TODO The task\n"
            (org-back-to-heading t)
            (let ((org-attach-auto-tag nil))
              (cl-letf (((symbol-function 'org-attach-dir)
                         (lambda (&rest _) directory)))
                (should (equal (envoy-org--attach-directory) directory))
                (should-not (member "ATTACH" (org-get-tags nil t)))))))
      (delete-directory directory t))))

(ert-deftest envoy-test-org-heading-saves-attachment-tag ()
  "Creating an attachment directory persists its ATTACH tag."
  (let ((directory (make-temp-file "envoy-test-attach-" t))
        (spool (make-temp-file "envoy-test-claim-" t)))
    (unwind-protect
        (envoy-test--with-org-file
            "* TODO The task\n:PROPERTIES:\n:ID: saved-id\n:END:\n"
          (let ((envoy-provider 'test)
                (envoy-providers '((test :program "true" :name "test agent"
                                           :args nil :edit-args nil
                                           :deny-arg nil :model-arg "--model")))
                (envoy-org-spool-directory spool)
                (org-attach-auto-tag "ATTACH"))
            (cl-letf (((symbol-function 'org-attach-dir)
                       (lambda (&rest _) directory))
                      ((symbol-function 'envoy-run)
                       (lambda (&rest _keys) 'fake-process)))
              (envoy-org-heading)
              (should-not (buffer-modified-p))
              (with-temp-buffer
                (insert-file-contents file)
                (org-mode)
                (org-back-to-heading t)
                (should (member "ATTACH" (org-get-tags nil t)))))))
      (delete-directory directory t)
      (delete-directory spool t))))

(ert-deftest envoy-test-attach-directory-that-cannot-be-made-tags-nothing ()
  "A heading with no directory is not marked as having one."
  (envoy-test--with-org "* TODO The task\n"
    (org-back-to-heading t)
    (cl-letf (((symbol-function 'org-attach-dir)
               (lambda (&rest _) (error "No directory"))))
      (should-not (envoy-org--attach-directory))
      (should-not (member "ATTACH" (org-get-tags nil t))))))

(ert-deftest envoy-test-org-heading-refuses-a-buffer-without-a-file ()
  "A heading in a buffer with no file cannot be delegated."
  (envoy-test--with-org "* TODO The task\n"
    (should-error (envoy-org-heading) :type 'user-error)))

(ert-deftest envoy-test-org-heading-refuses-a-plain-buffer ()
  "The command refuses a buffer that is not org."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (envoy-org-heading) :type 'user-error)))

(ert-deftest envoy-test-org-heading-asks-for-direction-before-running ()
  "The subprocess dispatch asks for a direction and includes it in the brief."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "Test Agent"
                                   :args nil :edit-args nil
                                   :deny-arg nil :model-arg nil)))
          (events nil)
          prompt brief)
      (cl-letf (((symbol-function 'envoy--read-direction)
                 (lambda (question)
                   (push 'direction events)
                   (setq prompt question)
                   "Keep the API stable"))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'envoy-run)
                 (lambda (text _callback &rest _keys)
                   (push 'run events)
                   (setq brief text)
                   'fake-process)))
        (envoy-org-heading)
        (should (equal prompt "Direction for Test Agent: "))
        (should (equal (nreverse events) '(direction run)))
        (should (string-match-p
                 (regexp-quote
                  "## Direction from the user\n\nKeep the API stable\n\n")
                 brief))))))

(ert-deftest envoy-test-summary-is-filed-under-the-heading ()
  "What the agent said is recorded under the heading, in its own drawer."
  (envoy-test--with-org "* TODO The task\n:PROPERTIES:\n:ID: x\n:END:\nBody.\n"
    (org-back-to-heading t)
    (envoy-org--record (point-marker) (current-buffer) "Wrote the file.")
    (let ((text (buffer-string)))
      (should (string-match-p ":ENVOY:" text))
      (should (string-match-p "Wrote the file\\." text))
      ;; Below the properties, not inside them.
      (should (> (string-match ":ENVOY:" text) (string-match ":ID: x" text))))))

(ert-deftest envoy-test-empty-summary-is-not-filed ()
  "An agent that said nothing leaves no drawer behind."
  (envoy-test--with-org "* TODO The task\nBody.\n"
    (org-back-to-heading t)
    (envoy-org--record (point-marker) (current-buffer) "   ")
    (should-not (string-match-p ":ENVOY:" (buffer-string)))))

;;; Choosing an agent

(ert-deftest envoy-test-claude-can-deny-tools-and-reasonix-cannot ()
  "The table records which agent can be told not to run a tool.

This is not a detail.  Naming Read and Edit to Claude Code's
--allowed-tools did not stop it running a shell command, and neither did
reasonix's --allowed-tools; only --disallowed-tools took the tool away,
and reasonix has no equivalent.  So the two agents differ in what a
rewrite through them can do, and `envoy-rewrite' says so before it
starts rather than implying a guarantee that is not there."
  (should (envoy-can-deny-tools-p 'claude))
  (should-not (envoy-can-deny-tools-p 'reasonix))
  ;; And the deny option is the deny one, not the allow one.
  (should (equal (plist-get (envoy--provider 'claude) :deny-arg)
                 "--disallowed-tools")))

(ert-deftest envoy-test-both-agents-are-configured ()
  "Both agents are described well enough to be run."
  (dolist (agent '(claude reasonix))
    (let ((plist (envoy--provider agent)))
      (should (plist-get plist :program))
      (should (plist-get plist :name))
      ;; Non-interactive output, which is the whole basis of the package.
      (should (member "-p" (plist-get plist :args)))
      (should (member "json" (plist-get plist :args))))))

(ert-deftest envoy-test-model-is-passed-when-asked-for ()
  "A chosen model reaches the command line; otherwise nothing is passed.

The real Claude Code entry supplies :model-arg, so what it names is what
gets asserted, but its :program is replaced with one every machine has.
`envoy--command' resolves the program before it assembles anything else,
so a test naming a real agent passes only where that agent is installed,
and this suite is meant to need neither agent."
  (let* ((claude (copy-sequence (envoy--provider 'claude)))
         (envoy-providers (list (cons 'claude (plist-put claude :program "sh"))))
         (envoy-provider 'claude))
    (should (equal (plist-get claude :model-arg) "--model"))
    (let ((envoy-model nil))
      (should-not (member "--model" (envoy--command nil nil))))
    (let ((envoy-model "some-model"))
      (should (member "--model" (envoy--command nil nil)))
      (should (member "some-model" (envoy--command nil nil))))))

(ert-deftest envoy-test-model-is-left-out-when-the-provider-cannot-take-one ()
  "A provider with no model option contributes no argument for the model.

The command line goes straight to `make-process', which refuses a list
holding nil, so appending an absent option is a crash rather than an
ignored setting."
  (let ((envoy-providers '((plain :program "true" :name "Plain"
                                  :args nil :edit-args nil
                                  :deny-arg nil :model-arg nil)))
        (envoy-provider 'plain)
        (envoy-model "some-model"))
    (let ((command (envoy--command nil nil)))
      (should-not (memq nil command))
      (should-not (member "some-model" command))
      (should (equal command (list (envoy--program nil)))))))

;;; A tmux window

;; No test here needs tmux installed, for the reason no test needs an agent
;; installed.  A shell script stands in for it, records every argument it was
;; given, and answers `has-session' from a list the test controls, so which
;; session a heading routes to can be asserted without a tmux server anywhere.

(defvar envoy-test--tmux-log nil
  "File the tmux stand-in writes its arguments to.")

(defun envoy-test--tmux (&optional live-sessions root)
  "Write a script that behaves like tmux and return its file name.
It logs every invocation to `envoy-test--tmux-log', answers has-session
successfully for the names in LIVE-SESSIONS and not for others, and
prints a target for new-window.  When ROOT is non-nil, keep the script and
log below that temporary root."
  (let* ((script-prefix (if root
                            (expand-file-name "tmux-" root)
                          "envoy-test-tmux-"))
         (log-prefix (if root
                         (expand-file-name "tmux-log-" root)
                       "envoy-test-tmux-log-"))
         (script (make-temp-file script-prefix nil ".sh"))
         (log (make-temp-file log-prefix)))
    (setq envoy-test--tmux-log log)
    (with-temp-file script
      (insert "#!/bin/sh\n"
              ;; One line per invocation, arguments separated by tabs, so an
              ;; argument holding spaces or newlines stays one field.
              (format "printf '%%s\\t' \"$@\" >> %s\n" log)
              (format "printf '\\n' >> %s\n" log)
              "case \"$1\" in\n"
              "  has-session)\n"
              ;; $3 arrives as =name; the leading = is dropped before matching.
              "    want=$(printf '%s' \"$3\" | sed 's/^=//')\n"
              (format "    for s in %s; do\n"
                      (mapconcat #'shell-quote-argument live-sessions " "))
              "      [ \"$s\" = \"$want\" ] && exit 0\n"
              "    done\n"
              "    exit 1 ;;\n"
              "  new-window) echo 'session:7' ;;\n"
              ;; What is running in the pane.  Not a shell, so the wait
              ;; before a typed prompt ends at once.
              "  capture-pane) echo 'an interface' ;;\n"
              "  display-message) echo 'true' ;;\n"
              "esac\n"
              "exit 0\n"))
    (set-file-modes script #o755)
    (push script envoy-test--scripts)
    script))

(defun envoy-test--tmux-calls ()
  "Return the tmux stand-in's invocations, each a list of arguments."
  (when (and envoy-test--tmux-log (file-exists-p envoy-test--tmux-log))
    (with-temp-buffer
      (insert-file-contents envoy-test--tmux-log)
      (mapcar (lambda (line) (split-string line "\t" t))
              (split-string (buffer-string) "\n" t)))))

(defmacro envoy-test--with-tmux (live-sessions &rest body)
  "Evaluate BODY with tmux replaced by a stand-in, LIVE-SESSIONS running."
  (declare (indent 1))
  `(let* ((root (make-temp-file "envoy-test-tmux-root-" t))
          (envoy-tmux-program (envoy-test--tmux ,live-sessions root))
          (envoy-tmux-provider 'claude)
          (envoy-org-spool-directory (expand-file-name "spool" root)))
     (unwind-protect (progn ,@body)
       (dolist (file envoy-test--scripts)
         (when (file-exists-p file) (delete-file file)))
       (setq envoy-test--scripts nil)
       (when (and envoy-test--tmux-log (file-exists-p envoy-test--tmux-log))
         (delete-file envoy-test--tmux-log))
       (setq envoy-test--tmux-log nil)
       (when (file-directory-p root)
         (delete-directory root t)))))
(ert-deftest envoy-test-tmux-window-failure-does-not-touch-attachments-first ()
  "A failed window leaves attachment setup untouched."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
      (let ((envoy-tmux-providers
             '((claude :program "true" :name "Claude"
                       :args () :prompt-arg nil :variable setup
                       :setup nil :setups ())))
            (events nil))
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _)
                     (push 'window events)
                     (user-error "Window failed")))
                  ((symbol-function 'envoy-org--attach-directory)
                   (lambda ()
                     (push 'attachment events)
                     nil)))
          (should-error (envoy-tmux-org-heading-to 'claude nil nil nil "test")
                        :type 'user-error))
        (should (equal events '(window)))))))

(ert-deftest envoy-test-tmux-sessions-are-matched-exactly ()
  "A session is asked for with tmux's exact-match prefix.

Without the `=' a target is a session id, then an exact name, then a
prefix of a name, then a glob.  Measured: a live session named
`project-work' answers to the target `project', and where two sessions
share a prefix the target is ambiguous and the command fails outright.
Every session name in this file comes from an org tag or a user option,
so every one of them can collide with another."
  (envoy-test--with-tmux '("project-work")
    (should (envoy-tmux-session-live-p "project-work"))
    (should-not (envoy-tmux-session-live-p "perm"))
    (let ((call (car (envoy-test--tmux-calls))))
      (should (equal (nth 0 call) "has-session"))
      (should (equal (nth 2 call) "=project-work")))))
(ert-deftest envoy-test-tmux-own-tag-beats-an-inherited-one ()
  "A dispatch uses the explicitly selected session, not heading tags."
  (envoy-test--with-tmux '("other-session" "project-work")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "project-work")))
      (should (equal (envoy-tmux-read-session 'claude) "project-work")))))
(ert-deftest envoy-test-tmux-session-name-carries-no-text-properties ()
  "The selected session is returned without minibuffer properties."
  (envoy-test--with-tmux '("picked")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (propertize "picked" 'face 'bold))))
      (let ((session (envoy-tmux-read-session 'claude)))
        (should (equal session "picked"))
        (should-not (text-properties-at 0 session))))))

(ert-deftest envoy-test-tmux-inherited-tag-routes-when-there-is-no-own-tag ()
  "A heading tag does not bypass the explicit session picker."
  (envoy-test--with-tmux '("other-session")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "other-session")))
      (should (equal (envoy-tmux-read-session 'claude) "other-session")))))
(ert-deftest envoy-test-tmux-falls-back-when-no-session-is-running ()
  "An empty session answer is rejected rather than inferred."
  (envoy-test--with-tmux '("other-session")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "")))
      (should-error (envoy-tmux-read-session 'claude) :type 'user-error))))
(ert-deftest envoy-test-tmux-tags-can-be-restricted-and-renamed ()
  "A session may be selected independently of heading tags."
  (envoy-test--with-tmux '("2026_project-work")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "2026_project-work")))
      (should (equal (envoy-tmux-read-session 'claude) "2026_project-work")))))


(ert-deftest envoy-test-tmux-window-name-is-trimmed-and-sanitised ()
  "A heading becomes a window name tmux and the status line can live with."
  (let ((envoy-tmux-window-name-width 40))
    (should (equal (envoy-tmux-window-name "Write the copy") "Write-the-copy"))
    (should (equal (envoy-tmux-window-name "  spaces  and\ttabs  ")
                   "spaces-and-tabs"))
    (should (equal (envoy-tmux-window-name "quote\"dollar$brace}")
                   "quote-dollar-brace-"))
    (let ((name (envoy-tmux-window-name (make-string 100 ?x))))
      (should (<= (string-width name) 40)))))

(ert-deftest envoy-test-tmux-window-target-is-an-index ()
  "The window is asked for by index, not by name.
Two headings with the same title are ordinary -- a datetree collects
them -- and a target naming a window by name would then be ambiguous."
  (envoy-test--with-tmux '("project-work")
    (should (equal (envoy-tmux-window "project-work" "The task") "session:7"))
    (let ((new-window (seq-find (lambda (call)
                                  (equal (car call) "new-window"))
                                (envoy-test--tmux-calls))))
      (should new-window)
      (should (member "#{session_name}:#{window_index}" new-window))
      (should (member "=project-work" new-window)))))
(ert-deftest envoy-test-tmux-session-is-started-only-when-absent ()
  "A live session check uses exact tmux status and preserves its result."
  (let (calls)
    (cl-letf (((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (if (equal args '("has-session" "-t" "=live"))
                     '(0 . "")
                   '(1 . "")))))
      (should (envoy-tmux-session-live-p "live"))
      (should-not (envoy-tmux-session-live-p "missing"))
      (should (= (length calls) 2)))))

(ert-deftest envoy-test-tmux-window-race-retries-after-duplicate-session ()
  "A session appearing during creation receives the competing window."
  (let ((live-results '(nil t))
        calls)
    (cl-letf (((symbol-function 'envoy-tmux-session-live-p)
               (lambda (_session) (pop live-results)))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (if (equal (car args) "new-session")
                     (cons 1 "duplicate session")
                   (cons 0 "session:7")))))
      (should (equal (envoy-tmux-window "competing" "The task")
                     "session:7")))
    (should (seq-find (lambda (call)
                        (equal (car call) "new-session"))
                      calls))
    (let ((new-window (seq-find (lambda (call)
                                 (equal (car call) "new-window"))
                               calls)))
      (should new-window)
      (should (member "=competing" new-window)))
    (should-not (seq-find (lambda (call)
                            (equal (car call) "kill-session"))
                          calls))))


(ert-deftest envoy-test-tmux-command-delivers-the-prompt-by-provider ()
  "Each agent receives the brief through its configured transport."
  (let* ((providers
          (mapcar (lambda (entry)
                    (cons (car entry)
                          (plist-put (copy-sequence (cdr entry))
                                     :program "true")))
                  envoy-tmux-providers))
         (envoy-tmux-providers providers)
         (file "/tmp/envoy-prompt-xyz"))
    (should-not (string-match-p "envoy-prompt-xyz"
                                (envoy-tmux-command file 'claude)))
    ;; pi reads a file named on the command line.
    (should (string-match-p "@/tmp/envoy-prompt-xyz"
                            (envoy-tmux-command file 'pi)))
    ;; reasonix takes neither, and is sent the brief as typed input instead.
    (should-not (string-match-p "envoy-prompt-xyz"
                                (envoy-tmux-command file 'reasonix)))))


(ert-deftest envoy-test-tmux-command-quotes-what-comes-from-outside ()
  "A model or a setup command reaches the shell as one argument.
These are read from the minibuffer, so a space or a semicolon in one has
to stay inside its own argument rather than starting a second command."
  (let* ((claude (copy-sequence (envoy-tmux--provider 'claude)))
         (envoy-tmux-providers
          (list (cons 'claude (plist-put claude :program "true")))))
    (let ((command (envoy-tmux-command "/tmp/p" 'claude "a b; rm -rf /" nil)))
      (should (string-match-p "--model" command))
      (should-not (string-match-p "; rm -rf /" command)))
    ;; A setup command is the exception, and deliberately so: it is a shell
    ;; command, offered by name from a user option, and quoting it would stop
    ;; it being one.
    (should (string-prefix-p "provider-wrapper && "
                             (envoy-tmux-command "/tmp/p" 'claude nil
                                                 "provider-wrapper")))))

(ert-deftest envoy-test-tmux-command-quotes-system-prompt-file ()
  "The provider system argument receives one shell-quoted file path."
  (let ((envoy-tmux-providers
         '((omp :program "true" :name "OMP" :args ()
                :prompt-arg "@%s" :system-arg "--append-system-prompt=%s"
                :model-arg "--model" :variable model :setup nil :models nil))))
    (let ((command
           (envoy-tmux-command "/tmp/prompt" 'omp nil nil nil
                               "/tmp/system prompt")))
      (should
       (string-match-p
        (regexp-quote
         (concat "--append-system-prompt="
                 (shell-quote-argument "/tmp/system prompt")))
        command)))))

(ert-deftest envoy-test-tmux-prompt-file-is-private ()
  "The brief is written where only its owner can read it.
A brief carries whatever the heading and its ancestors carry: a client's
name, a budget, a deadline.  The directory named by the variable
`temporary-file-directory' is shared on a shared machine."
  (let ((file (envoy-tmux--write-prompt "a brief")))
    (unwind-protect
        (progn
          (should (equal (file-modes file) #o600))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "a brief"))))
      (delete-file file))))

(ert-deftest envoy-test-tmux-typed-prompt-arrives-whole ()
  "A brief typed into the terminal is sent in full, in pieces.
An org subtree is longer than a command line may be, so it goes in
chunks -- and every chunk has to arrive, in order, or the agent reads a
brief with a hole in it."
  (envoy-test--with-tmux '("project-work")
    (let ((text (mapconcat #'number-to-string (number-sequence 1 4000) " ")))
      (should (> (length text) (* 2 envoy-tmux--type-chunk)))
      (envoy-tmux-type "session:7" text nil t)
      (let* ((calls (envoy-test--tmux-calls))
             (typed (seq-filter (lambda (call) (member "-l" call)) calls))
             (joined (mapconcat (lambda (call) (car (last call))) typed "")))
        (should (> (length typed) 1))
        (should (equal joined text))
        ;; And it is submitted, once, after the last piece.
        (should (equal (car (last (car (last calls)))) "Enter"))))))
(ert-deftest envoy-test-tmux-typed-prompt-chunks-by-utf8-bytes ()
  "A multibyte brief is split by encoded bytes, not characters."
  (envoy-test--with-tmux '("project-work")
    (let ((text (make-string 3000 ?λ))
          (envoy-tmux-type-pause 0))
      (envoy-tmux-type "session:7" text nil t)
      (let* ((calls (envoy-test--tmux-calls))
             (typed (seq-filter (lambda (call) (member "-l" call)) calls))
             (chunks (mapcar (lambda (call) (car (last call))) typed)))
        (should (> (string-bytes text) envoy-tmux--type-chunk))
        (should (> (length chunks) 1))
        (should (equal (apply #'concat chunks) text))
        (dolist (chunk chunks)
          (should (<= (string-bytes chunk) envoy-tmux--type-chunk)))))))

(ert-deftest envoy-test-tmux-typing-waits-for-the-agent-and-paces-itself ()
  "The brief is not typed until something other than a shell is reading.

This is the one thing the stand-in above cannot show, so it is asserted
here on the calls instead.  A nine kilobyte brief typed at a real pane in
one go arrived a kilobyte short, and short at the front: the head of it
went into the terminal's input queue while the shell was still starting
the agent, and nothing was draining it.  Asserting that every piece
reached tmux is therefore not enough -- it did, and the brief still lost
its first thirteen lines.  What has to hold is that the pane is asked
what it is running first, and that the pieces are spaced."
  (envoy-test--with-tmux '("project-work")
    (let ((envoy-tmux-type-pause 0)
          (envoy-tmux-start-timeout 0.5))
      (envoy-tmux-type "session:7" (make-string 9000 ?x) nil t)
      (let* ((calls (envoy-test--tmux-calls))
             (asked (seq-find (lambda (call)
                                (equal (car call) "display-message"))
                              calls))
             (typed (seq-filter (lambda (call) (member "-l" call)) calls)))
        ;; The pane is asked what it is running, and asked before anything
        ;; is typed at it.
        (should asked)
        (should (member "#{pane_current_command}" asked))
        (should (< (seq-position calls asked)
                   (seq-position calls (car typed))))
        ;; And no single piece is larger than the terminal's queue.
        (dolist (call typed)
          (should (<= (length (car (last call))) 4096)))))))
(ert-deftest envoy-test-tmux-typing-waits-for-configured-provider-after-setup ()
  "A typed brief waits while an unrelated setup process is still running."
  (let ((probe-results '("python" "claude"))
        (observed nil)
        (calls nil)
        (envoy-tmux-start-timeout 0.5))
    (cl-letf (((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (if (equal (car args) "display-message")
                     (let ((result (pop probe-results)))
                       (push result observed)
                       (cons 0 result))
                   (cons 0 "")))))
      (envoy-tmux-type "session:7" "the brief" "claude" t))
    (let ((probes (seq-filter (lambda (call)
                               (equal (car call) "display-message"))
                             calls)))
      (should (= (length probes) 2))
      (should (equal (car observed) "claude")))))

(ert-deftest envoy-test-tmux-typing-aborts-when-agent-is-not-running ()
  "Typing a brief signals an error when the agent never leaves the shell."
  (let ((calls nil))
    (cl-letf (((symbol-function 'envoy-tmux--wait-for-agent)
               (lambda (&rest _) nil))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (cons 0 "zsh"))))
      (should-error (envoy-tmux-type "session:7" "dangerous\ncommand")
                    :type 'user-error))
    (should-not
     (seq-some (lambda (call)
                 (and (equal (car call) "send-keys")
                      (member "-l" call)))
               calls))))
(ert-deftest envoy-test-tmux-typing-aborts-with-empty-shell-list ()
  "Typing a brief signals an error when no shell commands are configured."
  (let ((calls nil)
        (envoy-tmux-shells nil)
        (envoy-tmux-start-timeout 0.01))
    (cl-letf (((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (cons 0 "bash"))))
      (should-error (envoy-tmux-type "session:7" "dangerous\ncommand")
                    :type 'user-error))
    (should-not
     (seq-some (lambda (call)
                 (and (equal (car call) "send-keys")
                      (member "-l" call)))
               calls))))
(ert-deftest envoy-test-tmux-typing-refuses-shell-named-provider-with-empty-shell-list ()
  "Typing a brief signals an error when the expected provider is still a shell."
  (let ((calls nil)
        (envoy-tmux-shells nil)
        (envoy-tmux-start-timeout 0.01))
    (cl-letf (((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (cons 0 "bash"))))
      (should-error (envoy-tmux-type "session:7" "dangerous\ncommand"
                                     "bash" t)
                    :type 'user-error))
    (should-not
     (seq-some (lambda (call)
                 (and (equal (car call) "send-keys")
                      (member "-l" call)))
               calls))))
(ert-deftest envoy-test-tmux-typing-aborts-on-send-failure ()
  "A failed tmux send signals an error instead of submitting the prompt."
  (let ((calls nil))
    (cl-letf (((symbol-function 'envoy-tmux--wait-for-agent)
               (lambda (&rest _) t))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (if (equal (car args) "display-message")
                     (cons 0 "agent")
                     (cons 1 "Send failed")))))
      (should-error (envoy-tmux-type "session:7" "dangerous\ncommand")
                    :type 'user-error))
    (should-not
     (seq-some (lambda (call)
                 (and (equal (car call) "send-keys")
                      (member "Enter" call)))
               calls))))

(ert-deftest envoy-test-tmux-send-starts-the-agent-then-types-if-it-must ()
  "An agent without a prompt argument is typed at after it starts."
  (envoy-test--with-tmux '("project-work")
    (let* ((claude (copy-sequence (envoy-tmux--provider 'claude)))
           (envoy-tmux-providers
            (list (cons 'claude (plist-put claude :program "true"))))
           (envoy-tmux-type-pause 0))
      (let ((file (envoy-tmux-send "session:7" "the brief" 'claude)))
        (unwind-protect
            (let* ((calls (envoy-test--tmux-calls))
                   (typed (seq-filter (lambda (call) (member "-l" call)) calls)))
              ;; The command line, then the brief, each with its own Enter,
              ;; and the pane asked what it is running in between.
              (should (= (length typed) 2))
              (should (equal (car (last (nth 1 typed))) "the brief"))
              (should (seq-find (lambda (call)
                                  (equal (car call) "display-message"))
                                calls)))
          (when (and file (file-exists-p file)) (delete-file file))))))
  (envoy-test--with-tmux '("project-work")
    (let* ((reasonix (copy-sequence (envoy-tmux--provider 'reasonix)))
           (envoy-tmux-providers
            (list (cons 'reasonix (plist-put reasonix :program "true"))))
           (envoy-tmux-type-pause 0))
      (let ((file (envoy-tmux-send "session:7" "the brief" 'reasonix)))
        (unwind-protect
            (let* ((calls (envoy-test--tmux-calls))
                   (typed (seq-filter (lambda (call) (member "-l" call)) calls)))
              ;; The command line, then the brief, each with its own Enter,
              ;; and the pane asked what it is running in between.
              (should (= (length typed) 2))
              (should (equal (car (last (nth 1 typed))) "the brief"))
              (should (seq-find (lambda (call)
                                  (equal (car call) "display-message"))
                                calls)))
          (when (and file (file-exists-p file)) (delete-file file)))))))

(ert-deftest envoy-test-tmux-send-waits-for-typed-prompt-completion ()
  "A typed handoff returns only after its final chunk and Enter."
  (let ((timer nil)
        (calls nil)
        (claude (copy-sequence (envoy-tmux--provider 'claude))))
    (cl-letf (((symbol-function 'envoy-tmux--wait-for-agent)
               (lambda (&rest _) t))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (cons 0 "")))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (setq timer (cons function args))
                 'fake-timer))
              ((symbol-function 'sleep-for)
               (lambda (&rest _)
                 (when timer
                   (let ((pending timer))
                     (setq timer nil)
                     (apply (car pending) (cdr pending)))))))
      (let ((envoy-tmux-providers
             (list (cons 'claude (plist-put claude :program "true"))))
            (envoy-tmux-type-pause 0))
        (envoy-tmux-send "session:7" (make-string 9000 ?x) 'claude)))
    (let ((typed (seq-filter (lambda (call) (member "-l" call)) calls)))
      (should (> (length typed) 2))
      (should (equal (car (last (car calls))) "Enter")))))
(ert-deftest envoy-test-tmux-send-propagates-deferred-typed-prompt-failure ()
  "A failure in a later typed chunk reaches the tmux sender."
  (let ((timer nil)
        (typed-count 0)
        (claude (copy-sequence (envoy-tmux--provider 'claude))))
    (cl-letf (((symbol-function 'envoy-tmux--wait-for-agent)
               (lambda (&rest _) t))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (if (and (equal (car args) "send-keys")
                          (member "-l" args))
                     (progn
                       (setq typed-count (1+ typed-count))
                       (if (= typed-count 3)
                           (cons 1 "deferred send failed")
                         (cons 0 "")))
                   (cons 0 ""))))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (setq timer (cons function args))
                 'fake-timer))
              ((symbol-function 'sleep-for)
               (lambda (&rest _)
                 (when timer
                   (let ((pending timer))
                     (setq timer nil)
                     (apply (car pending) (cdr pending)))))))
      (let ((envoy-tmux-providers
             (list (cons 'claude (plist-put claude :program "true"))))
            (envoy-tmux-type-pause 0))
        (should-error
         (envoy-tmux-send "session:7" (make-string 9000 ?x) 'claude)
         :type 'user-error)))))

(ert-deftest envoy-test-tmux-all-agents-are-configured ()
  "Every tmux agent is described well enough to be started."
  (dolist (agent '(claude reasonix pi codex omp dsh))
    (let ((plist (envoy-tmux--provider agent)))
      (should (plist-get plist :program))
      (should (plist-get plist :name))
      (when (plist-get plist :efforts)
        (should (plist-get plist :effort-arg)))
      (when-let* ((arg (plist-get plist :prompt-arg)))
        (should (string-match-p "%s" arg)))
      (when (plist-get plist :opening-input)
        (should-not (plist-get plist :prompt-arg))))))

(ert-deftest envoy-test-tmux-heading-prompts-before-attachment-setup ()
  "Declining to save stops tmux dispatch before attachment setup."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file
        "#+TODO: TODO DOING | DONE\n* TODO The task\n"
      (let ((attachment-called nil)
            (window-called nil))
        (goto-char (point-max))
        (insert "Unrelated local edit.\n")
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _) nil))
                  ((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _)
                     (setq window-called t)
                     "session:7"))
                  ((symbol-function 'envoy-org--attach-directory)
                   (lambda ()
                     (setq attachment-called t)
                     nil)))
          (let ((error-data
                 (should-error (envoy-tmux-org-heading-to 'claude)
                               :type 'user-error)))
            (should (equal (cadr error-data)
                           "Envoy: the buffer has unsaved changes"))))
        (should (buffer-modified-p))
        (should-not window-called)
        (should-not attachment-called)))))
(ert-deftest envoy-test-tmux-org-heading-refuses-a-plain-buffer ()
  "The tmux command asks for an org buffer, like the subprocess one."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (envoy-tmux-org-heading-to 'claude) :type 'user-error)))

(ert-deftest envoy-test-tmux-org-heading-asks-direction-before-selection-and-session ()
  "The tmux heading asks for direction before selection or session."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n:PROPERTIES:\n:ID: direction-order\n:END:\n"
    (let ((envoy-tmux-providers
           '((claude :program "true" :name "Claude" :args ()
                     :prompt-arg nil :variable setup :setup nil :setups ())))
          (events nil)
          (selection-saw-claim nil)
          sent-claim prompt)
      (let* ((key (envoy-org--spool-key (org-entry-get nil "ID")))
             (claim-file
              (envoy-org--spool-file key envoy-org--spool-claim-suffix)))
        (cl-letf (((symbol-function 'envoy--read-direction)
                   (lambda (question)
                     (should (equal question "Direction for Claude: "))
                     (push 'direction events)
                     "Keep the API stable"))
                  ((symbol-function 'envoy-tmux--read-variable)
                   (lambda (&rest _)
                     (push 'selection events)
                     (setq selection-saw-claim (file-exists-p claim-file))
                     '(:model nil :setup nil :plan-into nil)))
                  ((symbol-function 'envoy-tmux-read-session)
                   (lambda (&rest _)
                     (push 'session events)
                     "work"))
                  ((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _)
                     (push 'window events)
                     "work:7"))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (_target text &optional _provider _model _setup
                                     _plan-into on-start claim &rest _)
                     (push 'send events)
                     (setq prompt text sent-claim claim)
                     (when on-start (funcall on-start))))
                  ((symbol-function 'envoy-org--attach-directory)
                   (lambda () nil))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (envoy-tmux-org-heading-to 'claude nil nil nil nil t))
        (should (equal (nreverse events)
                       '(direction selection session window send)))
        (should selection-saw-claim)
        (should (equal (plist-get sent-claim :key) key))
        (should (plist-get sent-claim :token))
        (should (string-match-p
                 (regexp-quote
                  "## Direction from the user\n\nKeep the API stable\n\n")
                 prompt))))))

(ert-deftest envoy-test-tmux-org-heading-blank-direction-uses-default ()
  "A blank tmux heading direction puts the default into the brief."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n:PROPERTIES:\n:ID: direction-default\n:END:\n"
    (let ((envoy-tmux-providers
           '((claude :program "true" :name "Claude" :args ()
                     :prompt-arg nil :variable setup :setup nil :setups ())))
          prompt)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (question)
                   (should (equal question "Direction for Claude: "))
                   " \t "))
                ((symbol-function 'envoy-tmux-read-session)
                 (lambda (&rest _) "work"))
                ((symbol-function 'envoy-tmux-window)
                 (lambda (&rest _) "work:8"))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (_target text &optional _provider _model _setup
                                   _plan-into on-start _claim &rest _)
                   (setq prompt text)
                   (when on-start (funcall on-start))))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (envoy-tmux-org-heading-to 'claude))
      (should (string-match-p
               (regexp-quote
                (concat "## Direction from the user\n\n"
                        envoy-default-direction "\n\n"))
               prompt)))))

(ert-deftest envoy-test-tmux-org-heading-direction-cancellation-releases-claim ()
  "Canceling direction input releases the heading claim before tmux starts."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n:PROPERTIES:\n:ID: direction-cancel\n:END:\n"
    (let ((envoy-tmux-providers
           '((claude :program "true" :name "Claude" :args ()
                     :prompt-arg nil :variable setup :setup nil :setups ())))
          (window-called nil))
      (let* ((key (envoy-org--spool-key (org-entry-get nil "ID")))
             (claim-file
              (envoy-org--spool-file key envoy-org--spool-claim-suffix))
             (state-file
              (envoy-org--spool-file key envoy-org--spool-state-suffix)))
        (cl-letf (((symbol-function 'envoy--read-direction)
                   (lambda (&rest _) (signal 'quit nil)))
                  ((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _)
                     (setq window-called t)
                     "work:9")))
          (condition-case nil
              (envoy-tmux-org-heading-to 'claude)
            (quit nil)))
        (should-not (file-exists-p claim-file))
        (should-not (file-exists-p state-file))
        (should-not window-called)))))

(ert-deftest envoy-test-tmux-keys-are-bound-to-the-three-agents ()
  "The three keys reach the three agents.
`envoy-org-setup-keys' takes the same first key, so whichever of the two
setup functions is called last wins it -- which is the point of them
being separate functions."
  (let ((org-mode-map (make-sparse-keymap)))
    (envoy-tmux-setup-keys)
    (should (eq (lookup-key org-mode-map (kbd "C-c C-x A"))
                #'envoy-tmux-org-heading-claude))
    (should (eq (lookup-key org-mode-map (kbd "C-c C-x R"))
                #'envoy-tmux-org-heading-reasonix))
    (should (eq (lookup-key org-mode-map (kbd "C-c C-x P"))
                #'envoy-tmux-org-heading-pi))))
(ert-deftest envoy-test-tmux-empty-setup-selection-skips-provider-default ()
  "An empty setup choice does not replace the provider's command."
  (let* ((provider (plist-put (copy-sequence (envoy-tmux--provider 'claude))
                              :program "true"))
         (envoy-tmux-providers (list (cons 'claude provider)))
         (command (envoy-tmux-command "/tmp/brief" 'claude nil "")))
    (should (string-match-p (regexp-quote (executable-find "true")) command))
    (should-not (string-match-p "nil" command))))

(ert-deftest envoy-test-tmux-command-prefixes-omp-brief-heading ()
  "An OMP command includes the brief heading before its @file argument."
  (let ((envoy-tmux-providers
         '((omp :program "true" :name "OMP" :args () :prompt-arg "@%s"
                :model-arg "--model" :variable model :setup nil :models ()))))
    (cl-letf (((symbol-function 'envoy-tmux--brief-heading)
               (lambda (_) "# Act: Ship it")))
      (should (string-match-p
               (regexp-quote "\\#\\ Act\\:\\ Ship\\ it")
               (envoy-tmux-command "/tmp/brief" 'omp))))))
(ert-deftest envoy-test-tmux-omp-picker-offers-only-native-modes ()
  "The OMP prefix picker returns native model selections and commands."
  (let ((envoy-tmux-providers
         '((omp :program "true" :name "OMP" :args ("--extra" "value")
                :prompt-arg "@%s" :model-arg "--model"
                :variable model :setup nil :models ())))
        mode-choices
        (responses '("Single model" "single" "Plan into" "planner"
                     "executor")))
    (cl-letf (((symbol-function 'envoy-tmux--omp-models)
               (lambda () '("single" "planner" "executor")))
              ((symbol-function 'completing-read)
               (lambda (prompt collection &rest _)
                 (when (string-match-p "launch mode" prompt)
                   (push collection mode-choices))
                 (pop responses))))
      (let* ((decode-command
              (lambda (command)
                (with-temp-buffer
                  (should
                   (zerop
                    (process-file "/bin/sh" nil t nil "-c"
                                  (concat "set -- " command
                                          "; printf '%s\\n' \"$@\""))))
                  (split-string (buffer-string) "\n" t))))
             (single (envoy-tmux--read-variable 'omp))
             (single-command
              (envoy-tmux-command "/tmp/brief" 'omp
                                  (plist-get single :model)
                                  (plist-get single :setup)
                                  (plist-get single :plan-into)))
             (single-argv (funcall decode-command single-command))
             (plan (envoy-tmux--read-variable 'omp))
             (plan-command
              (envoy-tmux-command "/tmp/brief" 'omp
                                  (plist-get plan :model)
                                  (plist-get plan :setup)
                                  (plist-get plan :plan-into)))
             (plan-argv (funcall decode-command plan-command)))
        (should (equal (nreverse mode-choices)
                       '(("Single model" "Plan into")
                         ("Single model" "Plan into"))))
        (should (equal single
                       '(:model "single" :setup nil :plan-into nil :effort nil)))
        (should (equal single-argv
                       (list (executable-find "true") "--extra" "value"
                             "--model" "single" "@/tmp/brief")))
        (should (equal plan
                       '(:model "planner" :setup nil
                         :plan-into "executor" :effort nil)))
        (should (equal plan-argv
                       (list (executable-find "true") "--extra" "value"
                             "--model" "planner" "--plan-yolo"
                             "--plan-yolo-into" "executor" "@/tmp/brief")))))))

(ert-deftest envoy-test-tmux-omp-picker-offers-thinking-effort ()
  "The OMP picker passes selected effort and preserves defaults on blank input."
  (let ((envoy-tmux-providers
         '((omp :program "true" :name "OMP" :args ("--extra" "value")
                :prompt-arg "@%s" :model-arg "--model"
                :effort-arg "--thinking" :efforts ("low" "high" "max")
                :variable model :setup nil :models ())))
        (responses '("Single model" "single" "high")))
    (cl-letf (((symbol-function 'envoy-tmux--omp-models)
               (lambda () '("single" "planner" "executor")))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (pop responses))))
      (let* ((decode-command
              (lambda (command)
                (with-temp-buffer
                  (should
                   (zerop
                    (process-file "/bin/sh" nil t nil "-c"
                                  (concat "set -- " command
                                          "; printf '%s\\n' \"$@\""))))
                  (split-string (buffer-string) "\n" t))))
             (single (envoy-tmux--read-variable 'omp))
             (single-argv
              (funcall decode-command
                       (envoy-tmux-command "/tmp/brief" 'omp
                                           (plist-get single :model)
                                           (plist-get single :setup)
                                           (plist-get single :plan-into)
                                           nil (plist-get single :effort)))))
        (should (equal single
                       '(:model "single" :setup nil :plan-into nil :effort "high")))
        (should (equal single-argv
                       (list (executable-find "true") "--extra" "value"
                             "--model" "single" "--thinking" "high"
                             "@/tmp/brief"))))
      (setq responses '("Plan into" "planner" "executor" ""))
      (let* ((plan (envoy-tmux--read-variable 'omp))
             (plan-argv
              (funcall
               (lambda (command)
                 (with-temp-buffer
                   (should
                    (zerop
                     (process-file "/bin/sh" nil t nil "-c"
                                   (concat "set -- " command
                                           "; printf '%s\\n' \"$@\""))))
                   (split-string (buffer-string) "\n" t)))
               (envoy-tmux-command "/tmp/brief" 'omp
                                   (plist-get plan :model)
                                   (plist-get plan :setup)
                                   (plist-get plan :plan-into)
                                   nil (plist-get plan :effort)))))
        (should (equal plan
                       '(:model "planner" :setup nil
                         :plan-into "executor" :effort nil)))
        (should-not (member "--thinking" plan-argv))))))

(ert-deftest envoy-test-tmux-dsh-picker-offers-single-and-plan-into ()
  "The dsh prefix picker offers Single model and Plan into over its models."
  (let ((envoy-tmux-providers
         '((dsh :program "true" :name "dsh" :args ()
                :prompt-arg "@%s" :model-arg "--model"
                :effort-arg "--thinking" :efforts ("off" "low" "high" "max")
                :variable model :setup nil
                :models ("deepseek-flash" "deepseek-v4-pro"))))
        mode-choices
        (responses '("Single model" "deepseek-v4-pro" "high"
                     "Plan into" "deepseek-flash" "deepseek-v4-pro" "")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest _)
                 (when (string-match-p "launch mode" prompt)
                   (push collection mode-choices))
                 (pop responses))))
      (should (equal (envoy-tmux--read-variable 'dsh)
                     '(:model "deepseek-v4-pro" :setup nil
                       :plan-into nil :effort "high")))
      (should (equal (envoy-tmux--read-variable 'dsh)
                     '(:model "deepseek-flash" :setup nil
                       :plan-into "deepseek-v4-pro" :effort nil)))
      (should (equal mode-choices
                     '(("Single model" "Plan into")
                       ("Single model" "Plan into")))))))

(ert-deftest envoy-test-tmux-dsh-single-model-names-model-and-effort ()
  "A dsh launch passes its model and thinking effort to dsh-launch."
  (let ((envoy-tmux-providers
         '((dsh :program "true" :name "dsh" :args ()
                :prompt-arg "@%s" :model-arg "--model"
                :effort-arg "--thinking" :efforts ("off" "low" "high" "max")
                :variable model :setup nil
                :models ("deepseek-flash" "deepseek-v4-pro")))))
    (should (equal (envoy-tmux-command "/tmp/brief" 'dsh "deepseek-v4-pro"
                                       nil nil nil "high")
                   (format "%s --model deepseek-v4-pro --thinking high @/tmp/brief"
                           (executable-find "true"))))))

(ert-deftest envoy-test-tmux-dsh-plan-into-names-planner-and-executor ()
  "A dsh plan-into launch names the planner and the executor to dsh-launch."
  (let ((envoy-tmux-providers
         '((dsh :program "true" :name "dsh" :args ()
                :prompt-arg "@%s" :model-arg "--model"
                :effort-arg "--thinking" :efforts ("off" "low" "high" "max")
                :variable model :setup nil
                :models ("deepseek-flash" "deepseek-v4-pro")))))
    (should (equal (envoy-tmux-command "/tmp/brief" 'dsh "deepseek-v4-pro"
                                       nil "deepseek-flash")
                   (format (concat "%s --model deepseek-v4-pro --plan-yolo"
                                   " --plan-yolo-into deepseek-flash @/tmp/brief")
                           (executable-find "true"))))))

(ert-deftest envoy-test-tmux-command-effort-validation-and-replacement ()
  "An explicit effort replaces configured flags and rejects invalid input."
  (let* ((provider '((omp :program "true" :name "OMP"
                           :args ("--thinking" "low")
                           :effort-arg "--thinking" :prompt-arg "@%s"
                           :model-arg "--model")))
         (envoy-tmux-providers provider)
         (command (envoy-tmux-command "/tmp/b" 'omp nil nil nil nil "high")))
    (should (equal (seq-filter (lambda (arg) (equal arg "--thinking"))
                               (split-string command))
                   '("--thinking")))
    (should (string-match-p "--thinking high" command))
    (let* ((envoy-tmux-providers
            '((omp :program "true" :name "OMP" :args ("--thinking=low")
                   :effort-arg "--thinking" :prompt-arg "@%s")))
           (command (envoy-tmux-command "/tmp/b" 'omp nil nil nil nil "high")))
      (should-not (string-match-p "--thinking=low" command))
      (should (string-match-p "--thinking high" command)))
    (should-error (envoy-tmux-command "/tmp/b" 'omp nil nil nil nil "")
                  :type 'user-error)
    (should-error (envoy-tmux-command "/tmp/b" 'claude nil nil nil nil "high")
                  :type 'user-error)))

(ert-deftest envoy-test-tmux-model-picker-rejects-an-empty-answer ()
  "An empty model selection is rejected before command construction."
  (let ((envoy-tmux-providers
         '((reasonix :program "true" :name "reasonix"
                    :variable model :models ()))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "")))
      (should-error (envoy-tmux--read-variable 'reasonix)
                    :type 'user-error))))

(ert-deftest envoy-test-tmux-agent-start-probe-respects-deadline ()
  "A hanging status probe cannot outlive the startup deadline.
The probe hangs for a full second against a deadline of a twentieth, so a
wait that honours the deadline returns in well under half a second and a
wait that does not takes the whole second.  The gap leaves room for a slow
CI runner without losing the distinction."
  (let ((envoy-tmux-start-timeout 0.05)
        called
        (started-at (float-time)))
    (cl-letf (((symbol-function 'envoy-tmux--call)
               (lambda (&rest _)
                 (setq called t)
                 (sleep-for 1.0)
                 (cons 0 "zsh"))))
      (should-not (envoy-tmux--wait-for-agent "session:7")))
    (should called)
    (should (< (- (float-time) started-at) 0.5))))
(ert-deftest envoy-test-tmux-agent-start-probe-accepts-wrapper-command ()
  "A configured provider may report a wrapper command while starting."
  (let ((envoy-tmux-start-timeout 0.05))
    (cl-letf (((symbol-function 'envoy-tmux--call)
               (lambda (&rest _)
                 (cons 0 "provider-wrapper"))))
      (should (envoy-tmux--wait-for-agent
               "session:7" "/opt/providers/provider-launcher")))))

(ert-deftest envoy-test-tmux-agent-start-probe-skips-setup-process ()
  "The readiness probe waits for the requested provider."
  (let ((probes 0)
        (envoy-tmux-start-timeout 0.1))
    (cl-letf (((symbol-function 'envoy-tmux--call)
               (lambda (&rest _args)
                 (setq probes (1+ probes))
                 (cons 0 (if (= probes 1) "sleep" "claude"))))
              ((symbol-function 'sleep-for)
               (lambda (&rest _args) nil)))
      (should (envoy-tmux--wait-for-agent "session:7" "claude")))
    (should (= probes 2))))
(ert-deftest envoy-tmux-test-command-uses-validated-program-path ()
  "A provider command launches from the path Emacs resolved."
  (let* ((directory (make-temp-file "envoy-test-provider-path-" t))
         (name (file-name-nondirectory
                (make-temp-name "envoy-test-provider-")))
         (program (expand-file-name name directory)))
    (unwind-protect
        (progn
          (with-temp-file program
            (insert "#!/bin/sh\nexit 0\n"))
          (set-file-modes program #o755)
          (let* ((envoy-tmux-providers
                  `((test :program ,name :name "test"
                          :args () :prompt-arg nil :model-arg nil)))
                 (exec-path (list directory))
                 (command (envoy-tmux-command "/tmp/envoy-test-prompt"
                                               'test)))
            (should (string-prefix-p (shell-quote-argument program) command))
            (should-not (string-prefix-p (shell-quote-argument name) command))
            (should (= 0 (call-process "/bin/sh" nil nil nil "-c"
                                       command)))))
      (delete-directory directory t))))

(ert-deftest envoy-tmux-test-rejects-empty-provider-program ()
  "An empty provider executable is rejected before command construction."
  (let ((envoy-tmux-providers
         '((test :program "" :name "test" :args () :prompt-arg nil))))
    (should-error (envoy-tmux-command "/tmp/envoy-test-prompt" 'test)
                  :type 'user-error)))
(ert-deftest envoy-test-tmux-heading-validates-provider-before-window ()
  "A missing provider program is rejected before dispatch prompts or runs."
  (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
    (let ((envoy-tmux-providers '((missing :program "" :name "Missing")))
          (direction-called nil)
          (window-called nil))
      (cl-letf (((symbol-function 'envoy--read-direction)
                 (lambda (&rest _)
                   (setq direction-called t)
                   (error "Direction must not be read")))
                ((symbol-function 'envoy-tmux-window)
                 (lambda (&rest _)
                   (setq window-called t)
                   "test:7")))
        (should-error
         (envoy-tmux-org-heading-to 'missing nil nil nil "test")
         :type 'user-error))
      (should-not direction-called)
      (should-not window-called))))


(ert-deftest envoy-test-tmux-window-failure-leaves-attachment-state-alone ()
  "Window or send failure restores attachment state."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
      (let ((org-attach-id-dir "attachments")
            (envoy-tmux-providers
             '((claude :program "true" :name "Claude"
                       :args () :prompt-arg nil :variable setup
                       :setup nil :setups ())))
            (attachment-root (expand-file-name "attachments" directory)))
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) (user-error "Window failed"))))
          (should-error (envoy-tmux-org-heading-to 'claude nil nil nil "test")
                        :type 'user-error))
        (should-not (file-directory-p attachment-root))
        (should-not (member "ATTACH" (org-get-tags nil t))))))
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
      (let ((org-attach-id-dir "attachments")
            (envoy-tmux-providers
             '((claude :program "true" :name "Claude"
                       :args () :prompt-arg nil :variable setup
                       :setup nil :setups ())))
            (attachment-root (expand-file-name "attachments" directory)))
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "session:7"))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (&rest _) (error "Send failed"))))
          (should-error (envoy-tmux-org-heading-to 'claude nil nil nil "test")))
        (should-not (file-directory-p attachment-root))
        (should-not (member "ATTACH" (org-get-tags nil t)))))))
(ert-deftest envoy-test-tmux-attachment-state-race-preserves-send-error ()
  "A state inspection race leaves the original send error intact."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
      (let ((org-attach-id-dir "attachments")
            (envoy-tmux-providers
             '((claude :program "true" :name "Claude"
                       :args () :prompt-arg nil :variable setup
                       :setup nil :setups ())))
            (state-calls 0)
            (attachment-root (expand-file-name "attachments" directory)))
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "session:7"))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (&rest _) (user-error "Send failed")))
                  ((symbol-function 'envoy-tmux--attachment-state)
                   (lambda (&rest _)
                     (setq state-calls (1+ state-calls))
                     (if (= state-calls 1)
                         '(saved-state)
                       (signal 'file-error '("attachment state raced"))))))
          (let ((error-data
                 (should-error (envoy-tmux-org-heading-to 'claude nil nil nil "test")
                               :type 'user-error)))
            (should (equal (cadr error-data) "Send failed"))))
        (should (= state-calls 2))
        (should (file-directory-p attachment-root))
        (should (member "ATTACH" (org-get-tags nil t)))))))
(ert-deftest envoy-test-tmux-attachment-snapshot-error-does-not-clean-up ()
  "An attachment snapshot race does not block dispatch or cleanup."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
      (let* ((org-attach-id-dir "attachments")
             (envoy-tmux-providers
              '((claude :program "true" :name "Claude"
                        :args () :prompt-arg nil :variable setup
                        :setup nil :setups ())))
             (send-called nil)
             (attachment-directory nil)
             (directory-files-calls 0)
             (original-attach-directory
              (symbol-function 'envoy-org--attach-directory))
             (original-directory-files (symbol-function 'directory-files)))
        (cl-letf (((symbol-function 'envoy-org--attach-directory)
                   (lambda ()
                     (setq attachment-directory
                           (funcall original-attach-directory))
                     attachment-directory))
                  ((symbol-function 'directory-files)
                   (lambda (&rest args)
                     (if (and attachment-directory
                              (equal (car args) attachment-directory)
                              (zerop directory-files-calls))
                         (progn
                           (setq directory-files-calls 1)
                           (signal 'file-error
                                   '("attachment directory disappeared")))
                       (apply original-directory-files args))))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (&rest _)
                     (setq send-called t)
                     (user-error "Send failed"))))
          (let ((error-data
                 (should-error (envoy-tmux-org-heading-to 'claude nil nil nil "test")
                               :type 'user-error)))
            (should (equal (cadr error-data) "Send failed"))))
        (should send-called)
        (should (= directory-files-calls 1))
        (should (file-directory-p attachment-directory))
        (should (member "ATTACH" (org-get-tags nil t)))))))
(ert-deftest envoy-test-tmux-unknown-attachment-snapshots-preserve-setup ()
  "Unknown initial and final attachment snapshots preserve setup on failure."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
      (let ((org-attach-id-dir "attachments")
            (envoy-tmux-providers
             '((claude :program "true" :name "Claude"
                       :args () :prompt-arg nil :variable setup
                       :setup nil :setups ())))
            (attachment-root (expand-file-name "attachments" directory)))
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "session:7"))
                  ((symbol-function 'envoy-tmux--attachment-state)
                   (lambda (&rest _) :unknown))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (&rest _) (error "Send failed"))))
          (should-error (envoy-tmux-org-heading-to 'claude nil nil nil "test")))
        (should (file-directory-p attachment-root))
        (should (member "ATTACH" (org-get-tags nil t)))))))

(ert-deftest envoy-test-tmux-window-failure-fixture-isolates-attachment-root ()
  "The cleanup fixture does not inherit an absolute attachment root."
  (let* ((outside (make-temp-file "envoy-test-attachment-root-" t))
         (org-attach-id-dir (expand-file-name "custom" outside))
         observed-root
         (original (symbol-function 'envoy-org--attach-directory)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'envoy-org--attach-directory)
                     (lambda (&rest args)
                       (setq observed-root org-attach-id-dir)
                       (apply original args))))
            (funcall
             (ert-test-body
              (ert-get-test
               'envoy-test-tmux-window-failure-leaves-attachment-state-alone))))
          (should (equal observed-root "attachments")))
      (delete-directory outside t))))
(ert-deftest envoy-test-tmux-prompt-failure-rolls-back-preparation ()
  "A prompt construction error rolls back attachment setup."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
      (let ((envoy-tmux-providers
             '((claude :program "true" :name "Claude"
                       :args () :prompt-arg nil :variable setup
                       :setup nil :setups ())))
            (attachment-root (expand-file-name "data" directory)))
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "session:7"))
                  ((symbol-function 'envoy-org-build-prompt)
                   (lambda (&rest _)
                     (error "Prompt construction failed"))))
          (let ((error-data
                 (should-error (envoy-tmux-org-heading-to 'claude nil nil nil "test")
                               :type 'error)))
            (should (equal (cadr error-data) "Prompt construction failed"))))
        (should-not (file-directory-p attachment-root))
        (should-not (member "ATTACH" (org-get-tags nil t)))))))
(ert-deftest envoy-test-tmux-prompt-failure-removes-tag-from-existing-attachment-directory ()
  "A prompt failure removes a new tag from an existing attachment directory."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
      (let* ((org-attach-auto-tag t)
             (org-attach-id-dir "attachments")
             (envoy-tmux-providers
              '((claude :program "true" :name "Claude"
                        :args () :prompt-arg nil :variable setup
                        :setup nil :setups ())))
             (attachment-directory
              (expand-file-name org-attach-id-dir directory)))
        (make-directory attachment-directory t)
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "session:7"))
                  ((symbol-function 'org-attach-dir)
                   (lambda (&rest _) attachment-directory))
                  ((symbol-function 'envoy-org-build-prompt)
                   (lambda (&rest _)
                     (error "Prompt construction failed"))))
          (let ((error-data
                 (should-error (envoy-tmux-org-heading-to 'claude nil nil nil "test")
                               :type 'error)))
            (should (equal (cadr error-data) "Prompt construction failed"))))
        (should (file-directory-p attachment-directory))
        (should-not (member "ATTACH" (org-get-tags nil t)))))))
(ert-deftest envoy-test-tmux-prompt-failure-rolls-back-configured-attachment-tag-on-disk ()
  "A prompt failure removes and saves a new configured attachment tag."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
      (let* ((org-attach-auto-tag "FILES")
             (org-attach-id-dir "attachments")
             (envoy-org-report-instructions nil)
             (envoy-tmux-providers
              '((claude :program "true" :name "Claude"
                        :args () :prompt-arg nil :variable setup
                        :setup nil :setups ())))
             (attachment-directory
              (expand-file-name org-attach-id-dir directory)))
        (make-directory attachment-directory t)
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "session:7"))
                  ((symbol-function 'org-attach-dir)
                   (lambda (&rest _) attachment-directory))
                  ((symbol-function 'envoy-org-build-prompt)
                   (lambda (&rest _)
                     (error "Prompt construction failed"))))
          (let ((error-data
                 (should-error (envoy-tmux-org-heading-to 'claude nil nil nil "test")
                               :type 'error)))
            (should (equal (cadr error-data) "Prompt construction failed"))))
        (should-not (member "FILES" (org-get-tags nil t)))
        (with-temp-buffer
          (insert-file-contents file)
          (should-not (string-match-p ":FILES:" (buffer-string))))))))
(defun envoy-test--workflow-checkout-guard-counts ()
  "Count each checkout step and its credential-persistence guards.
Reads workflow-step YAML from the current buffer, starting from point.
A step ends at the next list item whose dash sits at or above its own
indentation, not merely at the next `- uses:' line, so a plain `- name:'
or `- run:' step cannot inherit an open checkout step's guard window.
Returns a cons of (CHECKOUT-COUNT . GUARD-COUNT)."
  (let ((checkout-count 0)
        (guard-count 0)
        (checkout-indent nil))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (cond
         ((string-match "^\\([[:space:]]*\\)- uses: actions/checkout@" line)
          (setq checkout-count (1+ checkout-count)
                checkout-indent (length (match-string 1 line))))
         ((and checkout-indent
               (string-match "^\\([[:space:]]*\\)- " line)
               (<= (length (match-string 1 line)) checkout-indent))
          (setq checkout-indent nil))
         ((and checkout-indent
               (string-match-p
                "^[[:space:]]*persist-credentials: false$" line))
          (setq guard-count (1+ guard-count)
                checkout-indent nil))))
      (forward-line 1))
    (cons checkout-count guard-count)))

(ert-deftest envoy-test-workflow-checkout-guard-does-not-cross-step-boundary ()
  "A credential guard on a later step does not excuse an unguarded checkout."
  (with-temp-buffer
    (insert "jobs:\n"
            "  test:\n"
            "    steps:\n"
            "      - uses: actions/checkout@deadbeef\n"
            "      - name: unrelated\n"
            "        with:\n"
            "          persist-credentials: false\n")
    (goto-char (point-min))
    (let ((counts (envoy-test--workflow-checkout-guard-counts)))
      (should (= (car counts) 1))
      (should (= (cdr counts) 0)))))

(ert-deftest envoy-test-workflow-checkouts-are-read-only ()
  "CI workflows grant read-only contents access and do not persist checkout credentials."
  (let ((default-directory
         (or (locate-dominating-file default-directory "Makefile")
             default-directory)))
    (with-temp-buffer
      (insert-file-contents ".github/workflows/check.yml")
      (goto-char (point-min))
      (should (re-search-forward "^permissions:\n  contents: read$" nil t))
      (goto-char (point-min))
      (let ((counts (envoy-test--workflow-checkout-guard-counts)))
        (should (= (car counts) 2))
        (should (= (cdr counts) (car counts)))))))

(ert-deftest envoy-org-id-validation-counts-heading-after-drawer ()
  "A real heading after drawer content counts as a duplicate ID."
  (envoy-test--with-org-file "* TODO Same\n"
    (let* ((identity (envoy-org--heading-identity))
           (marker (point-marker))
           (buffer (current-buffer))
           (id (nth 0 identity)))
      (goto-char (point-max))
      (insert (format ":LOGBOOK:\n#+begin_foo\n:END:\n* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n"
                      id))
      (should-not (envoy-org--heading-id-matches-p marker buffer identity)))))
(ert-deftest envoy-org-id-validation-rejects-unterminated-drawer-before-duplicate ()
  "An unterminated drawer cannot hide a duplicate-ID heading."
  (envoy-test--with-org-file "* TODO Same\n"
    (let* ((identity (envoy-org--heading-identity))
           (marker (point-marker))
           (buffer (current-buffer))
           (id (nth 0 identity)))
      (goto-char (point-max))
      (insert (format ":NOTE:\n* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n"
                      id))
      (should-not (envoy-org--heading-id-matches-p marker buffer identity)))))
(ert-deftest envoy-org-id-validation-rejects-unterminated-block-before-duplicate ()
  "An unterminated block cannot hide a duplicate-ID heading."
  (envoy-test--with-org-file "* TODO Same\n"
    (let* ((identity (envoy-org--heading-identity))
           (marker (point-marker))
           (buffer (current-buffer))
           (id (nth 0 identity)))
      (goto-char (point-max))
      (insert (format "#+BEGIN_SRC text\n* TODO Same\n:PROPERTIES:\n:ID: %s\n:END:\n"
                      id))
      (should-not (envoy-org--heading-id-matches-p marker buffer identity)))))
(ert-deftest envoy-org-id-validation-scans-unterminated-block-linearly ()
  "An unterminated block suffix is scanned without rescanning each marker."
  (envoy-test--with-org-file "* TODO Same\n"
    (let* ((identity (envoy-org--heading-identity))
           (marker (point-marker))
           (buffer (current-buffer)))
      (goto-char (point-max))
      (insert (apply #'concat (make-list 64 "#+BEGIN_SRC text\n")))
      (let ((forward-line-calls 0)
            (original-forward-line (symbol-function 'forward-line)))
        (cl-letf (((symbol-function 'forward-line)
                   (lambda (&optional arg)
                     (setq forward-line-calls (1+ forward-line-calls))
                     (funcall original-forward-line arg))))
          (should (envoy-org--heading-id-matches-p marker buffer identity)))
        (should (< forward-line-calls 256))))))

(ert-deftest envoy-test-tmux-typing-keeps-pending-chunks-after-return ()
  "A deferred typed prompt continues after the initial call returns."
  (let ((pending nil)
        (cancelled nil)
        (calls nil))
    (cl-letf (((symbol-function 'envoy-tmux--wait-for-agent)
               (lambda (&rest _) t))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (cons 0 "")))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (setq pending (cons function args))
                 'fake-timer))
              ((symbol-function 'cancel-timer)
               (lambda (timer)
                 (setq cancelled timer))))
      (envoy-tmux-type "session:7" (make-string 9000 ?x))
      (should pending)
      (should-not cancelled)
      (while pending
        (let ((callback pending))
          (setq pending nil)
          (apply (car callback) (cdr callback)))))
    (let* ((typed (seq-filter (lambda (call) (member "-l" call)) calls))
           (joined (mapconcat (lambda (call) (car (last call))) typed "")))
      (should (> (length typed) 1))
      (should (equal joined (make-string 9000 ?x)))
      (should (equal (car (last (car calls))) "Enter")))))
(ert-deftest envoy-test-tmux-window-race-stays-fatal-when-session-is-absent ()
  "A failed session creation does not fall through to a window attempt."
  (let ((live-results '(nil nil))
        calls)
    (cl-letf (((symbol-function 'envoy-tmux-session-live-p)
               (lambda (_session) (pop live-results)))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (if (equal (car args) "new-session")
                     (cons 1 "duplicate session")
                   (cons 0 "session:7")))))
      (should-error (envoy-tmux-window "missing" "The task")
                    :type 'user-error))
    (should-not (seq-find (lambda (call)
                            (equal (car call) "new-window"))
                          calls))))
(ert-deftest envoy-org-record-preserves-block-literal-drawer-markers ()
  "A report drawer does not consume literal markers inside a source block."
  (envoy-test--with-org "* TODO The task\n#+BEGIN_SRC text\n:ENVOY:\nliteral\n:END:\n#+END_SRC\n"
    (org-back-to-heading t)
    (envoy-org--record (point-marker) (current-buffer) "New report.")
    (let ((text (buffer-string)))
      (should (string-match-p
               (concat (regexp-quote ":ENVOY:\n")
                       "[^\n]+"
                       (regexp-quote "\n: New report.\n:END:"))
               text))
      (should (string-match-p
               (regexp-quote
                "#+BEGIN_SRC text\n:ENVOY:\nliteral\n:END:\n#+END_SRC")
               text)))))


(ert-deftest envoy-org-record-leaves-a-later-sibling-alone ()
  "An unterminated drawer cannot reach past the end of its own subtree."
  (envoy-test--with-org
      "* TODO The task\n:ENVOY:\nOld report.\n* Sibling\n:END:\nBody.\n"
    (org-back-to-heading t)
    (envoy-org--record (point-marker) (current-buffer) "New report.")
    (let ((text (buffer-string)))
      (should (string-match-p (regexp-quote "New report.") text))
      (should (string-match-p (regexp-quote "* Sibling") text))
      (should (string-match-p (regexp-quote "Body.") text)))))

;;; Relocated from the run's own test files
;;
;; The run left these in test/makefile-test.el and test/envoy-tmux-test.el.
;; Neither file is loaded, so neither ran.  Two of them sat past
;; makefile-test.el's own end-of-file marker, which is how a passing suite
;; hid them.

(ert-deftest envoy-test-tmux-typed-prompt-wait-paces-synchronously ()
  "A waiting typed prompt pauses between chunks instead of polling."
  (let ((timer nil)
        (timer-count 0)
        (sleeps nil)
        (claude (copy-sequence (envoy-tmux--provider 'claude))))
    (cl-letf (((symbol-function 'envoy-tmux--wait-for-agent)
               (lambda (&rest _) t))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest _) (cons 0 "")))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (setq timer (cons function args)
                       timer-count (1+ timer-count))
                 'fake-timer))
              ((symbol-function 'sleep-for)
               (lambda (duration &rest _)
                 (push duration sleeps)
                 (when timer
                   (let ((pending timer))
                     (setq timer nil)
                     (apply (car pending) (cdr pending)))))))
      (let ((envoy-tmux-providers
             (list (cons 'claude (plist-put claude :program "true"))))
            (envoy-tmux-type-pause 0.25))
        (envoy-tmux-type "session:7" (make-string 9000 ?x) nil t)))
    (should (= (length sleeps) 2))
    (should (equal sleeps (make-list (length sleeps) 0.25)))
    (should (zerop timer-count))))

(ert-deftest envoy-test-review-queues-active-canonical-alias-result ()
  "A second alias result remains reviewable after the first review closes."
  (let* ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
         (alias-one (concat file "-one"))
         (alias-two (concat file "-two"))
         review-one review-two snapshot-one snapshot-two)
    (unwind-protect
        (progn
          (make-symbolic-link file alias-one)
          (make-symbolic-link file alias-two)
          (setq snapshot-one (envoy-snapshot alias-one))
          (with-temp-file file (insert "after one\n"))
          (setq review-one (envoy-review alias-one snapshot-one "first"))
          (should (buffer-live-p review-one))
          (setq snapshot-two (envoy-snapshot alias-two))
          (with-temp-file file (insert "after two\n"))
          (setq review-two (envoy-review alias-two snapshot-two "second" #'ignore))
          (should-not (buffer-live-p review-two))
          (should (file-exists-p snapshot-two))
          (with-current-buffer review-one
            (cl-letf (((symbol-function 'quit-window) #'ignore))
              (envoy-review-accept)))
          (setq review-two (get-buffer (envoy--diff-buffer-name alias-two)))
          (should (buffer-live-p review-two))
          (with-current-buffer review-two
            (should (string-match-p "second" (buffer-string)))
            (should (string-match-p "^[-+]after one\\|^[-+]after two"
                                    (buffer-string)))))
      (dolist (review (list review-one review-two))
        (when (buffer-live-p review)
          (with-current-buffer review (setq envoy-review--snapshot nil))
          (kill-buffer review)))
      (dolist (snapshot (list snapshot-one snapshot-two))
        (when (and snapshot (file-exists-p snapshot))
          (delete-file snapshot)))
      (dolist (alias (list alias-one alias-two))
        (when (file-exists-p alias) (delete-file alias)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest envoy-test-tmux-typing-aborts-on-empty-pane-command ()
  "Typing a brief signals an error when the pane has no current command."
  (let ((calls nil)
        (envoy-tmux-start-timeout 0.01))
    (cl-letf (((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (cons 0 ""))))
      (should-error (envoy-tmux-type "session:7" "dangerous\ncommand")
                    :type 'user-error))
    (should-not
     (seq-some (lambda (call)
                 (and (equal (car call) "send-keys")
                      (member "-l" call)))
               calls))))

(ert-deftest envoy-test-tmux-attachment-state-unknown-when-file-stat-fails ()
  "A failed file stat makes the attachment snapshot unknown."
  (let ((directory (make-temp-file "envoy-tmux-attachment-" t)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "* Heading\n")
          (goto-char (point-min))
          (setq buffer-file-name
                (expand-file-name "envoy-test-disappeared.org" directory))
          (cl-letf (((symbol-function 'file-exists-p)
                     (lambda (_) t))
                    ((symbol-function 'file-attributes)
                     (lambda (&rest _)
                       (signal 'file-error '("file disappeared")))))
            (should (eq :unknown
                        (envoy-tmux--attachment-state directory)))))
      (delete-directory directory t))))

(ert-deftest envoy-test-tmux-send-preserves-provider-path-for-readiness ()
  "Pass the configured provider path to the readiness helper."
  (let (typed)
    (cl-letf (((symbol-function 'envoy-tmux--provider)
               (lambda (&optional _provider)
                 '(:program "/opt/providers/provider-launcher"
                   :prompt-arg nil)))
              ((symbol-function 'envoy-tmux-command)
               (lambda (&rest _args) "/opt/providers/provider-launcher"))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest _args) '(0 . "")))
              ((symbol-function 'envoy-tmux-type)
               (lambda (&rest args) (setq typed args))))
      (envoy-tmux-send "session:7" "hello" 'provider)
      (should (equal "/opt/providers/provider-launcher"
                     (nth 2 typed))))))


(ert-deftest envoy-test-tmux-send-refuses-empty-or-placeholder-brief ()
  "A brief that is empty or only the placeholder never starts a session.

Sixteen dispatches ran real agents on a prompt file holding only the
word \"brief\" and billed them for work that did not exist, so the
refusal has to come before a prompt file is written or a key is sent,
for a provider that reads a file and for one that is typed at."
  (let ((writes nil)
        (envoy-tmux-providers
         '((omp :program "true" :name "OMP" :args () :prompt-arg "@%s"
                :system-arg "--append-system-prompt=%s"
                :model-arg "--model" :variable model :setup nil :models nil)
           (claude :program "true" :name "Claude Code" :args ()
                   :prompt-arg nil :model-arg "--model" :variable setup
                   :setup nil :setups ()))))
    (dolist (brief '("" "  \n " "brief" "Brief"))
      (dolist (provider '(omp claude))
        (cl-letf (((symbol-function 'envoy-tmux--write-prompt)
                   (lambda (text) (push text writes) "/tmp/envoy-test-stub"))
                  ((symbol-function 'envoy-tmux-type)
                   (lambda (_target text &rest _)
                     (push text writes)))
                  ((symbol-function 'envoy-tmux--call)
                   (lambda (&rest args) (push args writes) '(0 . ""))))
          (should-error (envoy-tmux-send "session:7" brief provider)
                        :type 'user-error))))
    (should (null writes))))

(ert-deftest envoy-test-tmux-send-uses-cmd-prompt-cleanup ()
  "A file-reading provider uses CMD syntax to remove its prompt file."
  (let ((calls nil)
        (envoy-tmux-providers
         '((provider
            :program "provider"
            :name "Provider"
            :args ()
            :prompt-arg "@%s"
            :model-arg nil
            :variable setup
            :setup nil
            :setups ())))
        file)
    (cl-letf (((symbol-function 'envoy-tmux-command)
               (lambda (prompt-file &rest _)
                 (format "provider @%s" prompt-file)))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (push args calls)
                 (if (equal (car args) "display-message")
                     '(0 . "cmd.exe")
                   '(0 . "")))))
      (setq file (envoy-tmux-send "session:7" "Do the task." 'provider)))
    (unwind-protect
        (let* ((send (seq-find
                      (lambda (call)
                        (and (equal (car call) "send-keys")
                             (member "-l" call)))
                      calls))
               (command (nth 4 send)))
          (should send)
          (should (string-match-p " & del /q \"" command))
          (should-not (string-match-p "; rm -f --" command)))
      (when (and file (file-exists-p file))
        (delete-file file)))))

(defmacro envoy-test--with-tmux-spool (&rest body)
  "Evaluate BODY with a temporary report spool whose path has spaces."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "envoy-test-tmux-spool-" t))
          (envoy-org-spool-directory
           (expand-file-name "spool with spaces" directory)))
     (unwind-protect
         (progn
           (make-directory envoy-org-spool-directory t)
           ,@body)
       (delete-directory directory t))))

(defun envoy-test--run-tmux-exit-fallback (claim)
  "Run CLAIM's generated fallback after a shell command fails."
  (let ((guard
         (cl-letf (((symbol-function 'envoy-tmux--call)
                    (lambda (&rest _) '(0 . "sh")))
                   )
           (envoy-tmux--exit-fallback "session:7" claim))))
    (should guard)
    (call-process "sh" nil nil nil "-c" (concat "false" guard))))

(ert-deftest envoy-test-tmux-exit-fallback-publishes-for-owned-claim ()
  "A failed provider exit publishes incomplete status for its own claim."
  (envoy-test--with-tmux-spool
    (let* ((claim (envoy-org--spool-claim "owned-run" "The task"))
           (failed (envoy-org--spool-file
                    (plist-get claim :key) envoy-org--spool-failed-suffix)))
      (should (zerop (envoy-test--run-tmux-exit-fallback claim)))
      (with-temp-buffer
        (insert-file-contents failed)
        (should (equal (buffer-string)
                       (concat "ENVOY_STATUS: incomplete\n"
                               "The agent exited without publishing a report.\n")))))))

(ert-deftest envoy-test-tmux-exit-fallback-skips-existing-done-marker ()
  "An existing completion marker prevents the exit fallback."
  (envoy-test--with-tmux-spool
    (let* ((claim (envoy-org--spool-claim "done-run" "The task"))
           (done (envoy-org--spool-file
                  (plist-get claim :key) envoy-org--spool-done-suffix))
           (failed (envoy-org--spool-file
                    (plist-get claim :key) envoy-org--spool-failed-suffix)))
      (with-temp-file done (insert "complete\n"))
      (should (zerop (envoy-test--run-tmux-exit-fallback claim)))
      (should-not (file-exists-p failed)))))

(ert-deftest envoy-test-tmux-exit-fallback-skips-existing-failed-marker ()
  "An existing failure marker remains untouched by the exit fallback."
  (envoy-test--with-tmux-spool
    (let* ((claim (envoy-org--spool-claim "failed-run" "The task"))
           (failed (envoy-org--spool-file
                    (plist-get claim :key) envoy-org--spool-failed-suffix)))
      (with-temp-file failed (insert "original marker\n"))
      (should (zerop (envoy-test--run-tmux-exit-fallback claim)))
      (with-temp-buffer
        (insert-file-contents failed)
        (should (equal (buffer-string) "original marker\n"))))))

(ert-deftest envoy-test-tmux-exit-fallback-skips-released-claim ()
  "The exit fallback does nothing after its claim has been released."
  (envoy-test--with-tmux-spool
    (let* ((claim (envoy-org--spool-claim "released-run" "The task"))
           (failed (envoy-org--spool-file
                    (plist-get claim :key) envoy-org--spool-failed-suffix)))
      (delete-file (plist-get claim :file))
      (should (zerop (envoy-test--run-tmux-exit-fallback claim)))
      (should-not (file-exists-p failed)))))

(ert-deftest envoy-test-tmux-exit-fallback-skips-foreign-claim ()
  "The exit fallback does nothing when another token owns the claim."
  (envoy-test--with-tmux-spool
    (let* ((claim (envoy-org--spool-claim "foreign-run" "The task"))
           (failed (envoy-org--spool-file
                    (plist-get claim :key) envoy-org--spool-failed-suffix)))
      (with-temp-file (plist-get claim :file)
        (prin1 '(:token "another-run") (current-buffer)))
      (should (zerop (envoy-test--run-tmux-exit-fallback claim)))
      (should-not (file-exists-p failed)))))

(ert-deftest envoy-test-org-collects-tmux-exit-fallback-as-incomplete ()
  "The collector files the fallback report and leaves its heading active."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n:PROPERTIES:\n:ID: tmux-exit-fallback\n:END:\n"
    (let* ((id (org-entry-get nil "ID"))
           (claim (envoy-org--spool-claim id "The task"))
           (key (plist-get claim :key))
           (failed (envoy-org--spool-file
                    key envoy-org--spool-failed-suffix)))
      (envoy-org--spool-open "The task" nil)
      (should (zerop (envoy-test--run-tmux-exit-fallback claim)))
      (should (= (envoy-org-collect-reports) 1))
      (should (equal (org-get-todo-state) "DOING"))
      (should-not (file-exists-p failed))
      (should (string-match-p "exited without publishing a report"
                              (buffer-string)))
      (should-not (string-match-p "ENVOY_STATUS" (buffer-string))))))

(ert-deftest envoy-test-tmux-omp-claim-sends-private-report-contract-and-cleans-it ()
  "OMP receives the report contract in a private file removed after exit."
  (envoy-test--with-tmux-spool
    (let ((envoy-tmux-providers
           (copy-tree envoy-tmux-providers))
          (calls nil)
          claim file system-file command)
      ;; The configured entry, with a stand-in program.  The launch line
      ;; this test builds is really executed below, so the configured
      ;; agent would really run, bill a session and need its API key in
      ;; CI.  Only the program is replaced; the arguments, the prompt
      ;; delivery and the report-contract delivery stay as configured.
      (let ((omp (assq 'omp envoy-tmux-providers)))
        (when omp
          (plist-put (cdr omp) :program "true")))
      (unwind-protect
          (progn
            (setq claim (envoy-org--spool-claim "omp-system" "The task"))
            (let ((writes nil)
                  (original-write
                   (symbol-function 'envoy-tmux--write-prompt)))
              (cl-letf (((symbol-function 'envoy-tmux--call)
                         (lambda (&rest args)
                           (if (equal (car args) "display-message")
                               '(0 . "sh")
                             (push args calls)
                             '(0 . ""))))
                        ((symbol-function 'envoy-tmux--write-prompt)
                         (lambda (text)
                           (let ((path (funcall original-write text)))
                             (push path writes)
                             path))))
                (setq file
                      (envoy-tmux-send "session:7" "Do the task." 'omp nil nil nil
                                       nil claim)))
              (setq system-file (car writes)))
            (let ((launch (seq-find
                           (lambda (call)
                             (and (equal (car call) "send-keys")
                                  (member "-l" call)))
                           calls)))
              (should launch)
              (setq command (nth 4 launch)))
            (should (string-match-p (regexp-quote (concat "@" file)) command))
            (should
             (string-match-p
              (regexp-quote
               (concat "--append-system-prompt="
                       (shell-quote-argument system-file)))
              command))
            (should (equal (file-modes system-file) #o600))
            (with-temp-buffer
              (insert-file-contents system-file)
              (should (string-match-p "ENVOY_STATUS: complete" (buffer-string)))
              (should
               (string-match-p
                (regexp-quote
                 (envoy-org--spool-file
                  (plist-get claim :key) envoy-org--spool-done-suffix))
                (buffer-string)))
              (should
               (string-match-p
                (regexp-quote
                 (envoy-org--spool-file
                  (plist-get claim :key) envoy-org--spool-failed-suffix))
                (buffer-string))))
            (should (zerop (call-process "sh" nil nil nil "-c" command)))
            (should-not (file-exists-p file))
            (should-not (file-exists-p system-file)))
        (dolist (temporary (list file system-file))
          (when (and temporary (file-exists-p temporary))
            (delete-file temporary)))))))

(ert-deftest envoy-test-tmux-send-without-claim-keeps-command-unchanged ()
  "A launch without a claim keeps legacy command and cleanup bytes."
  (envoy-test--with-tmux-spool
    (let ((envoy-tmux-providers
           '((omp :program "true" :name "OMP" :args () :prompt-arg "@%s"
                  :system-arg "--append-system-prompt=%s"
                  :model-arg "--model" :variable model :setup nil :models nil)))
          (calls nil)
          file command)
      (unwind-protect
          ;; The assertions run under the same stand-in that produced the
          ;; command.  The expected cleanup suffix asks tmux what shell the
          ;; pane runs, and a machine without tmux would answer nothing
          ;; here even though the dispatch itself never needed it.
          (cl-letf (((symbol-function 'envoy-tmux--call)
                     (lambda (&rest args)
                       (if (equal (car args) "display-message")
                           '(0 . "sh")
                         (push args calls)
                         '(0 . "")))))
            (setq file (envoy-tmux-send "session:7" "Do the task." 'omp))
            (let ((launch (seq-find
                           (lambda (call)
                             (and (equal (car call) "send-keys")
                                  (member "-l" call)))
                           calls)))
              (should launch)
              (setq command (nth 4 launch)))
            (should
             (equal
              command
              (concat
               (shell-quote-argument
                (envoy-tmux--provider-program 'omp))
               " " (shell-quote-argument "Do the task.")
               " " (format "@%s" (shell-quote-argument file))
               (envoy-tmux--prompt-file-cleanup "session:7" file))))
            (should-not (string-match-p "--append-system-prompt" command)))
        (when (and file (file-exists-p file))
          (delete-file file))))))

(provide 'envoy-test)

;;; envoy-test.el ends here

;;; Milestone feature coverage

(ert-deftest envoy-test-tmux-codex-provider-keeps-typed-brief-contract ()
  "Codex disables paste bursts and types the brief rather than using argv.
The two settings are delivery mechanics: a paste burst swallows the Enter,
and a command-line prompt arrives before the opening input can be sent."
  (let ((provider (envoy-tmux--provider 'codex)))
    (should (equal (plist-get provider :args)
                   '("-c" "disable_paste_burst=true")))
    (should-not (plist-get provider :prompt-arg))
    (should (equal (plist-get provider :opening-input) '("")))))
(ert-deftest envoy-test-tmux-codex-and-omp-commands-are-interactive ()
  "Codex, OMP and dsh expose interactive org commands on their own keys."
  (should (commandp #'envoy-tmux-org-heading-codex))
  (should (commandp #'envoy-tmux-org-heading-omp))
  (should (commandp #'envoy-tmux-org-heading-dsh))
  (let ((original-map (copy-keymap org-mode-map)))
    (with-temp-buffer
      (org-mode)
      (let ((org-mode-map (copy-keymap org-mode-map)))
        (envoy-tmux-setup-keys)
        (should (eq (lookup-key org-mode-map (kbd "C-c C-x C"))
                    #'envoy-tmux-org-heading-codex))
        (should (eq (lookup-key org-mode-map (kbd "C-c C-x S"))
                    #'envoy-tmux-org-heading-dsh))
        (should (eq (lookup-key org-mode-map (kbd "C-c C-x O"))
                    #'envoy-tmux-org-heading-omp))))
    (should (equal org-mode-map original-map))))


(ert-deftest envoy-test-tmux-omp-provider-reads-prompt-file-and-discovers-models ()
  "OMP reads a prompt file and exposes discovered model selectors."
  (let* ((provider (envoy-tmux--provider 'omp))
         (cache-file (make-temp-file "envoy-test-omp-cache-"))
         (envoy-tmux-omp-model-cache-file cache-file)
         (envoy-tmux-providers (copy-tree envoy-tmux-providers)))
    (unwind-protect
        (progn
          (plist-put (alist-get 'omp envoy-tmux-providers) :models nil)
          (should (equal (plist-get provider :prompt-arg) "@%s"))
          (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                     (lambda () "/fake/omp"))
                    ((symbol-function 'envoy-tmux--omp-discover-models)
                     (lambda (_program)
                       '("openai/gpt-5" "anthropic/claude"))))
            (should (equal (envoy-tmux--omp-models)
                           '("openai/gpt-5" "anthropic/claude")))))
      (delete-file cache-file))))

(ert-deftest envoy-test-tmux-omp-model-list-preserves-explicit-config ()
  "An explicit OMP model list avoids discovery and keeps its order."
  (let ((envoy-tmux-providers (copy-tree envoy-tmux-providers)))
    (plist-put (alist-get 'omp envoy-tmux-providers) :models
               '("first" "second"))
    (cl-letf (((symbol-function 'envoy-tmux--omp-program)
               (lambda () "/fake/omp"))
              ((symbol-function 'envoy-tmux--omp-discover-models)
               (lambda (_program) (error "Discovery should not run"))))
      (should (equal (envoy-tmux--omp-models) '("first" "second"))))))

(ert-deftest envoy-test-tmux-omp-models-answer-from-fresh-cache ()
  "A cache entry younger than the TTL answers without running OMP.
The discovery subprocess is what a contended OMP makes slow, so the
picker must be able to skip it altogether."
  (let* ((cache-file (make-temp-file "envoy-test-omp-cache-"))
         (envoy-tmux-omp-model-cache-file cache-file)
         (envoy-tmux-providers (copy-tree envoy-tmux-providers)))
    (unwind-protect
        (progn
          (plist-put (alist-get 'omp envoy-tmux-providers) :models nil)
          (with-temp-file cache-file
            (prin1 (list :program "/fake/omp"
                         :time (float-time)
                         :models '("cached/one" "cached/two"))
                   (current-buffer)))
          (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                     (lambda () "/fake/omp"))
                    ((symbol-function 'envoy-tmux--omp-discover-models)
                     (lambda (_program) (error "Discovery must not run"))))
            (should (equal (envoy-tmux--omp-models)
                           '("cached/one" "cached/two")))))
      (delete-file cache-file))))

(ert-deftest envoy-test-tmux-omp-models-fall-back-to-stale-cache ()
  "A failed discovery offers the stale cached list instead of refusing.
Both OMP queries timing out is the reported defect: the prefixed
dispatch aborted twice while several OMP sessions contended for the
state database."
  (let* ((cache-file (make-temp-file "envoy-test-omp-cache-"))
         (envoy-tmux-omp-model-cache-file cache-file)
         (envoy-tmux-providers (copy-tree envoy-tmux-providers))
         (messages nil))
    (unwind-protect
        (progn
          (plist-put (alist-get 'omp envoy-tmux-providers) :models nil)
          (with-temp-file cache-file
            (prin1 (list :program "/fake/omp"
                         :time (- (float-time) 7200)
                         :models '("stale/one"))
                   (current-buffer)))
          (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                     (lambda () "/fake/omp"))
                    ((symbol-function 'envoy-tmux--omp-discover-models)
                     (lambda (_program)
                       (user-error
                        "Envoy: OMP `models --json' timed out after 5 seconds")))
                    ((symbol-function 'message)
                     (lambda (&rest args)
                       (push (apply #'format args) messages))))
            (should (equal (envoy-tmux--omp-models) '("stale/one"))))
          (should (string-match-p
                   "Using the cached model list"
                   (car messages))))
      (delete-file cache-file))))

(ert-deftest envoy-test-tmux-omp-models-cache-a-successful-discovery ()
  "A successful discovery is recorded in the cache file for later calls."
  (let* ((cache-file (expand-file-name "envoy-test-omp-cache"
                                       temporary-file-directory))
         (envoy-tmux-omp-model-cache-file cache-file)
         (envoy-tmux-providers (copy-tree envoy-tmux-providers)))
    (when (file-exists-p cache-file) (delete-file cache-file))
    (unwind-protect
        (progn
          (plist-put (alist-get 'omp envoy-tmux-providers) :models nil)
          (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                     (lambda () "/fake/omp"))
                    ((symbol-function 'envoy-tmux--omp-discover-models)
                     (lambda (_program) '("fresh/one" "fresh/two"))))
            (should (equal (envoy-tmux--omp-models)
                           '("fresh/one" "fresh/two")))
            (should (equal (plist-get (envoy-tmux--omp-model-cache-read) :models)
                           '("fresh/one" "fresh/two")))
            (should (equal (plist-get (envoy-tmux--omp-model-cache-read) :program)
                           "/fake/omp"))))
      (delete-file cache-file))))

(ert-deftest envoy-test-tmux-omp-models-signal-without-cache ()
  "A failed discovery still refuses the dispatch when no cache exists."
  (let* ((cache-file (make-temp-file "envoy-test-omp-cache-"))
         (envoy-tmux-omp-model-cache-file cache-file)
         (envoy-tmux-providers (copy-tree envoy-tmux-providers)))
    (unwind-protect
        (progn
          (delete-file cache-file)
          (plist-put (alist-get 'omp envoy-tmux-providers) :models nil)
          (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                     (lambda () "/fake/omp"))
                    ((symbol-function 'envoy-tmux--omp-discover-models)
                     (lambda (_program)
                       (user-error "Envoy: OMP model discovery failed"))))
            (should-error (envoy-tmux--omp-models) :type 'user-error)))
      (when (file-exists-p cache-file) (delete-file cache-file)))))

(ert-deftest envoy-test-tmux-omp-models-ignore-a-foreign-program-cache ()
  "A cache entry for another program is not offered as a fallback."
  (let* ((cache-file (make-temp-file "envoy-test-omp-cache-"))
         (envoy-tmux-omp-model-cache-file cache-file)
         (envoy-tmux-providers (copy-tree envoy-tmux-providers)))
    (unwind-protect
        (progn
          (plist-put (alist-get 'omp envoy-tmux-providers) :models nil)
          (with-temp-file cache-file
            (prin1 (list :program "/other/omp"
                         :time (float-time)
                         :models '("other/one"))
                   (current-buffer)))
          (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                     (lambda () "/fake/omp"))
                    ((symbol-function 'envoy-tmux--omp-discover-models)
                     (lambda (_program)
                       (user-error "Envoy: OMP model discovery failed"))))
            (should-error (envoy-tmux--omp-models) :type 'user-error)))
      (delete-file cache-file))))


(ert-deftest envoy-test-tmux-omp-and-codex-heading-dispatchers-accept-prefixes ()
  "The two new heading commands pass optional prefixes to dispatch."
  (let (calls)
    (cl-letf (((symbol-function 'envoy-tmux--dispatch)
               (lambda (provider arg)
                 (push (list provider arg) calls))))
      (funcall #'envoy-tmux-org-heading-codex nil)
      (funcall #'envoy-tmux-org-heading-codex '(4))
      (funcall #'envoy-tmux-org-heading-omp nil)
      (funcall #'envoy-tmux-org-heading-omp '(4))
      (funcall #'envoy-tmux-org-heading-dsh nil)
      (funcall #'envoy-tmux-org-heading-dsh '(4))
      (should (equal (nreverse calls)
                     '((codex nil)
                       (codex (4))
                       (omp nil)
                       (omp (4))
                       (dsh nil)
                       (dsh (4))))))))

(ert-deftest envoy-test-org-whole-heading-stale-done-marker-stays-active ()
  "A done marker without status cannot complete the heading."
  (let ((spool (make-temp-file "envoy-test-spool-" t)))
    (unwind-protect
        (let ((envoy-org-spool-directory spool))
          (envoy-test--with-org-file
              "#+TODO: TODO DOING | DONE\n* TODO The task\nBody.\n"
            (let* ((key (envoy-org--spool-open "The task" nil))
                   (state (envoy-org--spool-file
                           key envoy-org--spool-state-suffix))
                   (done (envoy-org--spool-file
                          key envoy-org--spool-done-suffix)))
              (delete-file state)
              (with-temp-file done
                (insert "The run stopped before all work was complete.\n"))
              (should (= (envoy-org-collect-reports) 1))
              (should (equal (org-get-todo-state) "DOING"))
              (should (string-match-p "stopped before all work"
                                      (buffer-string)))
              (should-not (string-match-p "ENVOY_STATUS" (buffer-string)))
              (should-not (file-exists-p state))
              (should-not (file-exists-p done)))))
      (delete-directory spool t))))

(ert-deftest envoy-test-org-whole-heading-status-requires-exact-completion ()
  "Missing, invalid, or incomplete status keeps the heading active."
  (dolist (report
           '("ENVOY_STATUS: incomplete\nThe budget check remains unfinished."
             "The budget check remains unfinished."
             "ENVOY_STATUS: complete \nThe budget check remains unfinished."
             "ENVOY_STATUS: Complete\nThe budget check remains unfinished."
             "envoy_status: complete\nThe budget check remains unfinished."
             "The report starts with prose.\nENVOY_STATUS: complete\nThe budget check remains unfinished."
             "ENVOY_STATUS: complete\nThe budget check remains unfinished.\nENVOY_STATUS: incomplete"))
    (envoy-test--with-org-file
        "#+TODO: TODO DOING | DONE\n* TODO The task\n** TODO First part\n** TODO Budget check\n"
      (let ((envoy-provider 'test)
            (envoy-providers '((test :program "true" :name "test agent"
                                     :args nil :edit-args nil
                                     :deny-arg nil :model-arg nil)))
            (envoy-org-output-instructions nil)
            (envoy-org-outward-action-guard nil)
            callback)
        (cl-letf (((symbol-function 'envoy-run)
                   (lambda (_prompt cb &rest _keys)
                     (setq callback cb)
                     'fake-process))
                  ((symbol-function 'envoy-org--attach-directory)
                   (lambda () nil))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (envoy-org-heading)
          (funcall callback
                   (envoy--make-result :ok t :text report :status 0)))
        (goto-char (point-min))
        (re-search-forward "^\\* DOING The task$")
        (org-back-to-heading t)
        (should (equal (org-get-todo-state) "DOING"))
        (should (string-match-p "budget check remains" (buffer-string)))
        (should-not (string-match-p "ENVOY_STATUS" (buffer-string)))))))

(ert-deftest envoy-test-org-whole-heading-explicit-completion-reaches-done ()
  "An explicit whole-subtree completion moves the heading to DONE."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n** TODO First part\n** TODO Budget check\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "test agent"
                                   :args nil :edit-args nil
                                   :deny-arg nil :model-arg nil)))
          (envoy-org-output-instructions nil)
          (envoy-org-outward-action-guard nil)
          callback)
      (cl-letf (((symbol-function 'envoy-run)
                 (lambda (_prompt cb &rest _keys)
                   (setq callback cb)
                   'fake-process))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (envoy-org-heading)
        (funcall callback
                 (envoy--make-result
                  :ok t
                  :text (concat "ENVOY_STATUS: complete\n"
                                "Every requested deliverable and check in "
                                "the whole subtree is complete.")
                  :status 0)))
      (goto-char (point-min))
      (re-search-forward "^\\* DONE The task$")
      (org-back-to-heading t)
      (should (equal (org-get-todo-state) "DONE"))
      (should (string-match-p "whole subtree is complete"
                              (buffer-string)))
      (should-not (string-match-p "ENVOY_STATUS" (buffer-string))))))

(ert-deftest envoy-test-org-whole-heading-links-only-named-subprocess-output ()
  "The immediate result links only named files created after dispatch."
  (let ((directory (make-temp-file "envoy-test-output-links-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "inherited.txt" directory)
            (insert "project material\n"))
          (envoy-test--with-org-file
              "#+TODO: TODO DOING | DONE\n* TODO The task\n"
            (let ((envoy-provider 'test)
                  (envoy-providers '((test :program "true" :name "test agent"
                                           :args nil :edit-args nil
                                           :deny-arg nil :model-arg nil)))
                  (envoy-org-output-instructions nil)
                  (envoy-org-report-instructions nil)
                  (envoy-org-outward-action-guard nil)
                  callback)
              (cl-letf (((symbol-function 'envoy-org--attach-directory)
                         (lambda () directory))
                        ((symbol-function 'org-attach-dir)
                         (lambda (&rest _) directory))
                        ((symbol-function 'envoy-run)
                         (lambda (_prompt cb &rest _keys)
                           (setq callback cb)
                           'fake-process))
                        ((symbol-function 'message) (lambda (&rest _) nil)))
                (envoy-org-heading)
                (with-temp-file (expand-file-name "deliverable.txt" directory)
                  (insert "new result\n"))
                (with-temp-file (expand-file-name "scratch.txt" directory)
                  (insert "unmentioned output\n"))
                (funcall callback
                         (envoy--make-result
                          :ok t
                          :text (concat "ENVOY_STATUS: complete\n"
                                        "The deliverable.txt is ready. "
                                        "The inherited.txt file remains available.")
                          :status 0))
                (goto-char (point-min))
                (re-search-forward "^\\* DONE The task$")
                (org-back-to-heading t)
                (should (equal (org-get-todo-state) "DONE"))
                (should
                 (string-match-p
                  (regexp-quote
                   (org-link-make-string
                    "attachment:deliverable.txt"
                    (org-link-escape "deliverable.txt")))
                  (buffer-string)))
                (dolist (filename '("inherited.txt" "scratch.txt"))
                  (should-not
                   (string-match-p
                    (regexp-quote
                     (org-link-make-string
                      (concat "attachment:" filename)
                      (org-link-escape filename)))
                    (buffer-string))))))))
      (delete-directory directory t))))

(ert-deftest envoy-test-org-whole-heading-failed-transport-stays-active ()
  "A failed transport cannot complete a task even with a complete report."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n** TODO Remaining work\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "test agent"
                                   :args nil :edit-args nil
                                   :deny-arg nil :model-arg nil)))
          (envoy-org-output-instructions nil)
          (envoy-org-outward-action-guard nil)
          callback)
      (cl-letf (((symbol-function 'envoy-run)
                 (lambda (_prompt cb &rest _keys)
                   (setq callback cb)
                   'fake-process))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (envoy-org-heading)
        (funcall callback
                 (envoy--make-result
                  :ok nil
                  :text (concat "ENVOY_STATUS: complete\n"
                                "The transport failed before delivery.")
                  :status 1)))
      (goto-char (point-min))
      (re-search-forward "^\\* DOING The task$")
      (org-back-to-heading t)
      (should (equal (org-get-todo-state) "DOING"))
      (should (string-match-p "transport failed before delivery"
                              (buffer-string)))
      (should-not (string-match-p "ENVOY_STATUS" (buffer-string))))))

(ert-deftest envoy-test-org-whole-heading-custom-done-keyword-is-honored ()
  "An explicit complete status uses the configured done keyword."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | SHIPPED\n* TODO The task\n** TODO Deliverable\n"
    (let ((envoy-org-done-keyword "SHIPPED")
          (result
           (envoy--make-result
            :ok t
            :text (concat "ENVOY_STATUS: complete\n"
                          "Every subtree deliverable is complete.")
            :status 0)))
      (envoy-org--finish (point-marker) (current-buffer) result nil)
      (should (equal (org-get-todo-state) "SHIPPED")))))

(ert-deftest envoy-test-org-whole-heading-completion-advances-repeater ()
  "An explicit complete status keeps Org's repeat transition."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO Repeat\nSCHEDULED: <2026-09-19 Sat +1d>\n"
    (let ((result
           (envoy--make-result
            :ok t
            :text (concat "ENVOY_STATUS: complete\n"
                          "The repeated subtree is complete.")
            :status 0)))
      (envoy-org--finish (point-marker) (current-buffer) result nil)
      (should (equal (org-get-todo-state) "TODO"))
      (should (equal (org-entry-get nil "SCHEDULED")
                     "<2026-09-20 Sun +1d>")))))

(ert-deftest envoy-test-org-whole-heading-disabled-active-keyword-keeps-state ()
  "Disabling the active keyword leaves the current state unchanged."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n"
    (let ((envoy-org-active-keyword nil)
          (result (envoy--make-result
                   :ok t
                   :text "ENVOY_STATUS: incomplete\nThe task remains incomplete."
                   :status 0)))
      (envoy-org--finish (point-marker) (current-buffer) result nil)
      (should (equal (org-get-todo-state) "TODO")))))

(ert-deftest envoy-test-org-whole-heading-spooled-incomplete-stays-active ()
  "An incomplete report stays active and links its named new output."
  (let ((directory (make-temp-file "envoy-test-partial-output-" t)))
    (unwind-protect
        (envoy-test--with-org-file
            "#+TODO: TODO DOING | DONE\n* TODO The task\n"
          (cl-letf (((symbol-function 'org-attach-dir)
                     (lambda (&rest _) directory)))
            (let* ((before (envoy-org--attachment-files directory))
                   (key (envoy-org--spool-open "The task" before))
                   (done (envoy-org--spool-file
                          key envoy-org--spool-done-suffix))
                   (output (expand-file-name "partial-output.txt" directory)))
              (with-temp-file output (insert "unfinished output\n"))
              (envoy-test--publish-spool-marker
               done
               "The partial-output.txt file is usable, but the budget check remains unfinished."
               "incomplete")
              (should (= (envoy-org-collect-reports) 1))
              (should (equal (org-get-todo-state) "DOING"))
              (should (string-match-p "budget check remains unfinished"
                                      (buffer-string)))
              (should-not (string-match-p "ENVOY_STATUS" (buffer-string)))
              (should
               (string-match-p
                (regexp-quote
                 (org-link-make-string
                  "attachment:partial-output.txt"
                  (org-link-escape "partial-output.txt")))
                (buffer-string))))))
      (delete-directory directory t))))

(ert-deftest envoy-test-org-whole-heading-spooled-failure-stays-active ()
  "A failed marker cannot complete a task with a complete status."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n"
    (let* ((key (envoy-org--spool-open "The task" nil))
           (failed (envoy-org--spool-file key envoy-org--spool-failed-suffix)))
      (envoy-test--publish-spool-marker
       failed "The transport failed before delivery." "complete")
      (should (= (envoy-org-collect-reports) 1))
      (should (equal (org-get-todo-state) "DOING"))
      (should (string-match-p "transport failed before delivery"
                              (buffer-string)))
      (should-not (string-match-p "ENVOY_STATUS" (buffer-string))))))

(ert-deftest envoy-test-org-whole-heading-status-only-collection-replaces-drawer ()
  "A status-only completion replaces the old report with its current state."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* DOING The task\n:PROPERTIES:\n:ID: status-only-task\n:END:\n:ENVOY:\n  DOING\nThe prior run left unfinished work.\n:END:\nBody.\n"
    (let* ((key (envoy-org--spool-open "The task" nil))
           (done (envoy-org--spool-file
                  key envoy-org--spool-done-suffix)))
      (envoy-test--publish-spool-marker done "" "complete")
      (should (= (envoy-org-collect-reports) 1))
      (goto-char (point-min))
      (re-search-forward "^\\* DONE The task$")
      (org-back-to-heading t)
      (should (equal (org-get-todo-state) "DONE"))
      (let* ((drawer (envoy-org--drawer-bounds envoy-org-drawer))
             (drawer-text
              (and drawer
                   (buffer-substring-no-properties
                    (car drawer) (cdr drawer))))
             (persisted
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
        (should drawer)
        (should (string-match-p "  DONE\n" drawer-text))
        (should-not
         (string-match-p "The prior run left unfinished work" drawer-text))
        (should (string-match-p "^\\* DONE The task$" persisted))
        (should (string-match-p "  DONE\n" persisted))
        (should-not
         (string-match-p "The prior run left unfinished work" persisted))
        (should-not (string-match-p "ENVOY_STATUS" persisted))))))

(ert-deftest envoy-test-org-whole-heading-links-only-named-new-attachments ()
  "Collection links only existing, named entries outside the known baseline."
  (let ((attachment-root
         (make-temp-file "envoy-test-inherited-attachments-" t)))
    (unwind-protect
        (dolist
            (scenario
             '((missing-state
                ("catalog.txt") (("new-output.txt" file))
                "The new-output.txt file is ready." nil
                ("new-output.txt" "catalog.txt"))
               (empty-known
                nil (("empty-output.txt" file))
                "The empty-output.txt file is ready."
                ("empty-output.txt") nil)
               (preexisting
                ("existing.txt") nil
                "The existing.txt file remains useful." nil
                ("existing.txt"))
               (unmentioned
                nil (("unmentioned.txt" file) ("scratch-bundle" directory))
                "The task result is complete." nil
                ("unmentioned.txt" "scratch-bundle"))
               (named-new
                ("old-support.txt") (("deliverable.txt" file))
                "The deliverable.txt file contains the result."
                ("deliverable.txt") ("old-support.txt"))
               (substring
                nil (("result.txt" file))
                "The result.txtx file is not a deliverable."
                nil ("result.txt"))
               (period-punctuation
                nil (("period.txt" file))
                "See period.txt. It contains the final figures."
                ("period.txt") nil)
               (named-directory
                nil (("release-bundle" directory))
                "The release-bundle directory contains the deliverable."
                ("release-bundle") nil)))
          (let* ((name (nth 0 scenario))
                 (initial (nth 1 scenario))
                 (outputs (nth 2 scenario))
                 (summary (nth 3 scenario))
                 (linked (nth 4 scenario))
                 (unlinked (nth 5 scenario))
                 (directory (expand-file-name (symbol-name name)
                                              attachment-root))
                 (id (format "attachment-%s" name)))
            (make-directory directory t)
            (dolist (filename initial)
              (with-temp-file (expand-file-name filename directory)
                (insert "existing material\n")))
            (envoy-test--with-org-file
                (format "#+TODO: TODO DOING | DONE\n* TODO The task\n:PROPERTIES:\n:ID: %s\n:END:\n"
                        id)
              (cl-letf (((symbol-function 'org-attach-dir)
                         (lambda (&rest _) directory)))
                (let* ((before (envoy-org--attachment-files directory))
                       (key (envoy-org--spool-open "The task" before))
                       (state (envoy-org--spool-file
                               key envoy-org--spool-state-suffix))
                       (done (envoy-org--spool-file
                              key envoy-org--spool-done-suffix)))
                  (when (eq name 'missing-state)
                    (delete-file state))
                  (dolist (output outputs)
                    (let ((path (expand-file-name (car output) directory)))
                      (if (eq (cadr output) 'directory)
                          (make-directory path)
                        (with-temp-file path (insert "new output\n")))))
                  (envoy-test--publish-spool-marker done summary "complete")
                  (should (= (envoy-org-collect-reports) 1))
                  (goto-char (point-min))
                  (re-search-forward "^\\* DONE The task$")
                  (org-back-to-heading t)
                  (should (equal (org-get-todo-state) "DONE"))
                  (should-not (string-match-p "ENVOY_STATUS" (buffer-string)))
                  (dolist (filename linked)
                    (let ((link (org-link-make-string
                                 (concat "attachment:" filename)
                                 (org-link-escape filename))))
                      (should (string-match-p (regexp-quote link)
                                              (buffer-string)))))
                  (dolist (filename unlinked)
                    (let ((link (org-link-make-string
                                 (concat "attachment:" filename)
                                 (org-link-escape filename))))
                      (should-not (string-match-p (regexp-quote link)
                                                  (buffer-string))))))))))
      (delete-directory attachment-root t))))

(ert-deftest envoy-test-org-collect-rescans-a-missing-id-at-most-once-per-interval ()
  "An unresolvable report does not drive an org-id rescan every cycle."
  (let ((spool (make-temp-file "envoy-test-spool-" t)))
    (unwind-protect
        (let ((envoy-org-spool-directory spool)
              (envoy-org-missing-id-rescan-interval 3600)
              (envoy-org--missing-id-rescan-time 0)
              (rescans 0))
          (envoy-test--publish-spool-marker
           (expand-file-name "missing-id.done" spool)
           "A report for a heading that does not exist.\n")
          (cl-letf (((symbol-function 'org-id-find-id-file)
                     (lambda (_id) nil))
                    ((symbol-function 'org-id-update-id-locations)
                     (lambda (&rest _) (cl-incf rescans))))
            (envoy-org-collect-reports)
            (envoy-org-collect-reports)
            (should (= rescans 1))))
      (delete-directory spool t))))

(provide 'envoy-test)

;;; envoy-test.el ends here
(ert-deftest envoy-test-file-changed-p-distinguishes-new-and-rewritten-files ()
  "A new file is changed without a snapshot, while two absent files are not."
  (let ((absent (make-temp-name "envoy-test-absent-"))
        (created (make-temp-file "envoy-test-created-" nil ".txt"))
        (rewritten (make-temp-file "envoy-test-rewritten-" nil ".txt" "before\n"))
        snapshot)
    (unwind-protect
        (progn
          (delete-file created)
          (should-not (envoy--file-changed-p absent nil))
          (setq snapshot (envoy-snapshot rewritten))
          (with-temp-file rewritten
            (insert "after\n"))
          (should (envoy--file-changed-p rewritten snapshot))
          (with-temp-file created
            (insert "created\n"))
          (should (file-exists-p created))
          (should (envoy--file-changed-p created nil)))
      (when (and snapshot (file-exists-p snapshot))
        (delete-file snapshot))
      (when (file-exists-p absent)
        (delete-file absent))
      (when (file-exists-p created)
        (delete-file created))
      (when (file-exists-p rewritten)
        (delete-file rewritten)))))

(ert-deftest envoy-test-review-header-advertises-available-iteration ()
  "A review with an iteration callback advertises another pass."
  (let ((file (make-temp-file "envoy-test-iterate-" nil ".txt" "before\n"))
        snapshot review)
    (unwind-protect
        (progn
          (setq snapshot (envoy-snapshot file))
          (with-temp-file file
            (insert "after\n"))
          (setq review (envoy-review file snapshot "summary" #'ignore))
          (should (buffer-live-p review))
          (with-current-buffer review
            (should (string-match-p
                     "C-c C-c keep it   C-c C-k undo it   C-c C-i another pass"
                     (buffer-string)))))
      (when (buffer-live-p review)
        (with-current-buffer review
          (setq envoy-review--snapshot nil))
        (kill-buffer review))
      (when (and snapshot (file-exists-p snapshot))
        (delete-file snapshot))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest envoy-test-review-accept-preserves-edit-after-queued-result ()
  "Accepting an earlier review does not overwrite a later independent edit."
  (let* ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
         (alias-one (concat file "-one"))
         (alias-two (concat file "-two"))
         (envoy-review--queue nil)
         review-one queued-review snapshot-one snapshot-two)
    (unwind-protect
        (progn
          (make-symbolic-link file alias-one)
          (make-symbolic-link file alias-two)
          (setq snapshot-one (envoy-snapshot alias-one))
          (with-temp-file file (insert "after one\n"))
          (setq review-one (envoy-review alias-one snapshot-one "first"))
          (should (buffer-live-p review-one))
          (setq snapshot-two (envoy-snapshot alias-two))
          (with-temp-file file (insert "after two\n"))
          (should-not (envoy-review alias-two snapshot-two "second" #'ignore))
          (with-temp-file file (insert "independent\n"))
          (with-current-buffer review-one
            (cl-letf (((symbol-function 'quit-window) #'ignore))
              (envoy-review-accept)))
          (setq queued-review (get-buffer (envoy--diff-buffer-name alias-two)))
          (should (buffer-live-p queued-review))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) "independent\n"))))
      (dolist (review (list review-one queued-review))
        (when (buffer-live-p review)
          (with-current-buffer review (setq envoy-review--snapshot nil))
          (kill-buffer review)))
      (dolist (entry envoy-review--queue)
        (dolist (snapshot (list (nth 1 entry) (nth 5 entry)))
          (when (and snapshot (file-exists-p snapshot))
            (delete-file snapshot))))
      (setq envoy-review--queue nil)
      (dolist (snapshot (list snapshot-one snapshot-two))
        (when (and snapshot (file-exists-p snapshot))
          (delete-file snapshot)))
      (dolist (alias (list alias-one alias-two))
        (when (file-exists-p alias) (delete-file alias)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest envoy-test-envelope-rejects-non-string-result ()
  "A structured result payload is rejected before it reaches consumers."
  (let ((result
         (envoy--result-from
          0
          "{\"type\":\"result\",\"is_error\":false,\"result\":{\"message\":\"done\"}}"
          "")))
    (should-not (envoy-result-ok result))
    (should (stringp (envoy-result-text result)))
    (should (string-match-p "exited with status 0"
                            (envoy-result-text result)))))

(ert-deftest envoy-test-missing-program-calls-back-before-resignaling ()
  "A missing provider still reaches the failure callback."
  (let ((envoy-providers
         '((gone :program "envoy-no-such-program-xyz"
                 :name "Gone" :args nil :edit-args nil
                 :deny-arg nil :model-arg "--model")))
        (envoy-provider 'gone)
        (calls 0)
        result)
    (let ((error-message
           (cadr
            (should-error
             (envoy-run "do something"
                        (lambda (received)
                          (setq calls (1+ calls)
                                result received)))))))
      (should (equal calls 1))
      (should (envoy-result-p result))
      (should-not (envoy-result-ok result))
      (should (string-match-p "envoy-no-such-program-xyz"
                              (envoy-result-text result)))
      (should (string-match-p "envoy-no-such-program-xyz"
                              error-message)))))

(ert-deftest envoy-test-setup-failure-cleans-before-callback-and-returns-process ()
  "A setup send failure cleans up, calls back, and still returns the process."
  (let ((script (envoy-test--agent
                 "{\"type\":\"result\",\"is_error\":false,\"result\":\"unused\"}"))
        (created nil)
        (events nil)
        (callback-finished nil)
        (callback-live nil)
        (callback-calls 0)
        result)
    (envoy-test--with-agent script
      (let ((real-make-process (symbol-function 'make-process))
            (real-process-put (symbol-function 'process-put)))
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq created (apply real-make-process args))))
                  ((symbol-function 'process-put)
                   (lambda (process key value)
                     (when (eq key 'envoy-finished)
                       (setq events (append events (list :cleanup))))
                     (funcall real-process-put process key value)))
                  ((symbol-function 'process-send-string)
                   (lambda (&rest _) (error "Send failed"))))
          (let ((returned
                 (envoy-run
                  "do something"
                  (lambda (received)
                    (setq events (append events (list :callback))
                          callback-calls (1+ callback-calls)
                          callback-finished (process-get created 'envoy-finished)
                          callback-live (process-live-p created)
                          result received)))))
            (should (equal events '(:cleanup :callback)))
            (should (processp returned))
            (should-not (process-live-p returned))
            (should (equal callback-calls 1))
            (should callback-finished)
            (should-not callback-live)
            (should (envoy-result-p result))
            (should-not (envoy-result-ok result))))))))

(ert-deftest envoy-test-delegate-does-not-register-dead-setup-process ()
  "A dead setup-failure process is not registered or announced as live."
  (let ((file (make-temp-file "envoy-test-" nil ".txt" "before\n"))
        (messages nil)
        dead-process)
    (unwind-protect
        (progn
          (setq dead-process
                (make-process :name "envoy-test-dead"
                              :command '("cat")
                              :connection-type 'pipe
                              :noquery t))
          (delete-process dead-process)
          (let ((envoy--runs nil))
            (cl-letf (((symbol-function 'envoy-run)
                       (lambda (_prompt callback &rest _keys)
                         (funcall callback
                                  (envoy--make-result
                                   :ok nil :text "send failed"))
                         dead-process))
                      ((symbol-function 'envoy-review)
                       (lambda (&rest _) nil))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args)
                               messages))))
              (envoy-delegate "do something" file
                              :snapshot nil
                              :directory default-directory)
              (should-not envoy--runs)
              (should-not
               (seq-some (lambda (message)
                           (string-match-p "is working on" message))
                         messages)))))
      (when (and dead-process (process-live-p dead-process))
        (delete-process dead-process))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest envoy-test-tmux-stand-in-captures-a-stable-pane ()
  "The tmux stand-in returns a stable non-empty pane capture."
  (envoy-test--with-tmux '()
    (let ((first (cdr (envoy-tmux--call "capture-pane" "-p" "-t"
                                         "session:7")))
          (second (cdr (envoy-tmux--call "capture-pane" "-p" "-t"
                                          "session:7"))))
      (should (not (string-empty-p first)))
      (should (equal first second)))))

(ert-deftest envoy-test-tmux-interface-wait-consumes-changing-pane ()
  "A changing pane is read until two non-empty captures agree."
  (let ((captures '("" "half a frame" "the interface" "the interface"))
        (calls 0))
    (cl-letf (((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (should (equal (car args) "capture-pane"))
                 (setq calls (1+ calls))
                 (cons 0 (pop captures))))
              ((symbol-function 'sleep-for)
               (lambda (&rest _) nil)))
      (should (envoy-tmux--wait-for-interface "session:7")))
    (should (= calls 4))))

(ert-deftest envoy-test-tmux-interface-wait-ignores-empty-captures ()
  "Empty captures do not settle the interface wait."
  (let ((captures '("" "" "" "the interface" "the interface"))
        (calls 0))
    (cl-letf (((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (should (equal (car args) "capture-pane"))
                 (setq calls (1+ calls))
                 (cons 0 (pop captures))))
              ((symbol-function 'sleep-for)
               (lambda (&rest _) nil)))
      (should (envoy-tmux--wait-for-interface "session:7")))
    (should (= calls 5))))

(ert-deftest envoy-test-tmux-interface-wait-gives-up-on-never-settling-pane ()
  "A pane that keeps changing is abandoned at the draw timeout."
  (let ((now 0.0)
        (calls 0))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&rest _) now))
              ((symbol-function 'envoy-tmux--call)
               (lambda (&rest args)
                 (should (equal (car args) "capture-pane"))
                 (setq calls (1+ calls))
                 (cons 0 (format "frame-%d" calls))))
              ((symbol-function 'sleep-for)
               (lambda (&rest _) (setq now 1.0))))
      (let ((envoy-tmux-draw-timeout 0.1))
        (should-not (envoy-tmux--wait-for-interface "session:7"))))
    (should (= calls 1))))

(ert-deftest envoy-test-org-spool-state-files-are-private ()
  "Spool directories and state files use private modes."
  (let ((spool (make-temp-file "envoy-test-spool-modes-" t)))
    (unwind-protect
        (let ((envoy-org-spool-directory spool))
          (set-file-modes spool #o755)
          (let ((state (envoy-org--spool-write-state
                        "state-key" "id" nil)))
            (should (= (file-modes spool) #o700))
            (should (= (file-modes state) #o600)))
          (delete-directory spool t)
          (let ((state (envoy-org--spool-write-state
                        "new-key" "id" nil)))
            (should (= (file-modes spool) #o700))
            (should (= (file-modes state) #o600))))
      (when (file-directory-p spool)
        (delete-directory spool t)))))

(ert-deftest envoy-test-org-spool-reports-rejects-symlinked-files ()
  "The spool report lister ignores symlinked report files."
  (let* ((spool (make-temp-file "envoy-test-spool-links-" t))
         (good (expand-file-name "good.done" spool))
         (outside (make-temp-file "envoy-test-outside-" nil ".done"))
         (link (expand-file-name "evil.done" spool)))
    (unwind-protect
        (progn
          (envoy-test--publish-spool-marker good "safe report\n")
          (envoy-test--publish-spool-marker outside "outside report\n")
          (make-symbolic-link outside link)
          (let ((envoy-org-spool-directory spool))
            (should (equal (envoy-org--spool-reports)
                           (list (list "good" t good))))))
      (when (file-directory-p spool)
        (delete-directory spool t))
      (when (file-exists-p outside)
        (delete-file outside)))))

(ert-deftest envoy-test-org-attachment-paths-are-json-encoded ()
  "Attachment paths cannot add prompt structure as raw text."
  (let* ((path "line\n\"quoted\"")
         (encoded (json-encode-string path))
         (formatted (envoy-org--format-attachments
                     (list (list "Parent" "/tmp/attachments" (list path))))))
    (should (string-match-p (regexp-quote encoded) formatted))
    (envoy-test--with-org "* TODO The task\n"
      (let ((envoy-org-output-instructions nil)
            (envoy-org-report-instructions nil)
            (envoy-org-outward-action-guard nil))
        (let ((prompt (envoy-org-build-prompt
                       "The task" (envoy-org--subtree-text) nil (list path))))
          (should (string-match-p (regexp-quote encoded) prompt)))))))

(ert-deftest envoy-test-org-attachment-link-escapes-description ()
  "Attachment link descriptions preserve closing brackets."
  (let ((name "foo]bar"))
    (dolist (directory (list "/tmp/envoy-test-attachments" nil))
      (let* ((file (if directory
                       (expand-file-name name directory)
                     (expand-file-name name "/tmp")))
             (target (if directory
                         (concat "attachment:" name)
                       (concat "file:" file)))
             (expected (org-link-make-string target (org-link-escape name))))
        (should (equal (envoy-org--attachment-link file directory)
                       expected))))))

(ert-deftest envoy-test-org-heading-restores-keyword-on-startup-error ()
  "A synchronous provider error restores the heading's previous keyword."
  (envoy-test--with-org-file "#+TODO: TODO DOING | DONE\n* TODO The task\n"
    (let ((envoy-provider 'test)
          (envoy-providers '((test :program "true" :name "test agent"
                                   :args nil :edit-args nil
                                   :deny-arg nil :model-arg nil)))
          (envoy-org-output-instructions nil)
          (envoy-org-report-instructions nil)
          (envoy-org-outward-action-guard nil)
          error-data)
      (cl-letf (((symbol-function 'envoy-run)
                 (lambda (&rest _keys)
                   (signal 'error '("provider startup failed"))))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil)))
        (setq error-data (should-error (envoy-org-heading) :type 'error))
        (should (equal (cadr error-data) "provider startup failed"))
        (should (equal (org-get-todo-state) "TODO"))))))

(ert-deftest envoy-test-org-finish-labels-a-repeat-without-a-keyword ()
  "A keywordless repeating completion says it repeated and names no nil."
  (envoy-test--with-org "* TODO The task\n"
    (let ((envoy-org-done-keyword nil)
          (result (envoy--make-result
                   :ok t :text "ENVOY_STATUS: complete\nDone." :status 0)))
      (cl-letf (((symbol-function 'org-get-repeat)
                 (lambda () "+1w")))
        (envoy-org--finish (point-marker) (current-buffer) result nil)
        (should-not (string-match-p "nil, repeats" (buffer-string)))
        (should (string-match-p (regexp-quote "repeats +1w")
                                (buffer-string)))))))

(ert-deftest envoy-test-org-finish-keeps-a-bare-keyword-when-nothing-repeats ()
  "A heading that does not repeat is labelled with its keyword alone."
  (envoy-test--with-org "* TODO The task\n"
    (let ((envoy-org-done-keyword 'done)
          (result (envoy--make-result
                   :ok t :text "ENVOY_STATUS: complete\nDone." :status 0)))
      (cl-letf (((symbol-function 'org-get-repeat)
                 (lambda () nil)))
        (let ((keyword (envoy-org--finish
                        (point-marker) (current-buffer) result nil)))
          (should keyword)
          (should (string-match-p (regexp-quote keyword) (buffer-string)))
          (should-not (string-match-p "repeats" (buffer-string))))))))

;;; Notmuch show delegation

(defmacro envoy-test--with-notmuch-thread (message-id thread-query thread-text
                                             &rest body)
  "Evaluate BODY with MESSAGE-ID, THREAD-QUERY and THREAD-TEXT as mail data.
The optional notmuch package is not loaded."
  (declare (indent 3) (debug t))
  `(let ((notmuch-calls nil)
         (real-require (symbol-function 'require)))
     (cl-letf (((symbol-function 'require)
                (lambda (feature &optional filename noerror)
                  (if (eq feature 'notmuch-show)
                      t
                    (funcall real-require feature filename noerror))))
               ((symbol-function 'notmuch-show-get-message-id)
                (lambda (&optional _bare) ,message-id))
               ((symbol-function 'notmuch-show-get-query)
                (lambda () ,thread-query))
               ((symbol-function 'notmuch-command-to-string)
                (lambda (&rest args)
                  (push args notmuch-calls)
                  (cond
                   ((equal args
                           (list "search" "--output=threads" "--format=text"
                                 "--limit=2" "--exclude=false" "--"
                                 ,message-id))
                    (concat ,thread-query "\n"))
                   ((equal args
                           (list "show" "--format=text" "--entire-thread=true"
                                 "--exclude=false" "--" ,thread-query))
                    ,thread-text)
                   (t (error "Unexpected notmuch arguments %S" args))))))
       ,@body)))

(ert-deftest envoy-test-notmuch-delegate-dsh-sends-to-dsh ()
  "The dsh notmuch command hands the request to dsh, not to OMP."
  (with-temp-buffer
    (let ((major-mode 'notmuch-show-mode)
          (sent nil))
      (envoy-test--with-notmuch-thread
          "id:message-dsh" "thread:thread-dsh" "THREAD\n"
        (cl-letf (((symbol-function 'envoy--read-direction)
                   (lambda (&rest _) "Look at this"))
                  ((symbol-function 'envoy-tmux-read-session)
                   (lambda (provider)
                     (should (eq provider 'dsh))
                     "work"))
                  ((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "work:7"))
                  ((symbol-function 'envoy-tmux--provider-program)
                   (lambda (&rest _) "/fake/dsh-launch"))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (target _prompt &optional provider &rest _)
                     (setq sent (list target provider))))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (should (equal (envoy-notmuch-delegate-dsh) "work:7"))
          (should (equal sent '("work:7" dsh))))))))

(ert-deftest envoy-test-notmuch-region-precedes-thread-and-prompt-effects ()
  "The selected region stays primary after instruction input moves point.
The prompt also keeps the complete thread as supporting, untrusted context."
  (with-temp-buffer
    (insert "displayed before\nSELECTED region\ndisplayed after\n")
    (let ((major-mode 'notmuch-show-mode)
          (transient-mark-mode t)
          (sent nil)
          (window-called nil)
          (before-text (buffer-string)))
      (goto-char (point-min))
      (search-forward "SELECTED region")
      (let ((end (point)))
        (goto-char (- end (length "SELECTED region")))
        (set-mark end)
        (activate-mark)
        (envoy-test--with-notmuch-thread
            "id:message-1" "thread:thread-1"
            "FULL MESSAGE\nCOLLAPSED MESSAGE\n"
          (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                     (lambda () "/fake/omp"))
                    ((symbol-function 'envoy--read-direction)
                     (lambda (&rest _)
                       (deactivate-mark)
                       (goto-char (point-max))
                       "Inspect this"))
                    ((symbol-function 'envoy-tmux-read-session)
                     (lambda (provider)
                       (should (eq provider 'omp))
                       "work"))
                    ((symbol-function 'envoy-tmux-window)
                     (lambda (session title)
                       (setq window-called (list session title))
                       "work:7"))
                    ((symbol-function 'envoy-tmux-send)
                     (lambda (target prompt &optional provider model setup
                                      plan-into &rest _)
                       (setq sent (list target prompt provider model setup
                                        plan-into))))
                    ((symbol-function 'message) (lambda (&rest _) nil)))
            (should (equal (envoy-notmuch-delegate) "work:7"))
            (should window-called)
            (should sent)
            (let* ((prompt (nth 1 sent))
                   (selection-start (string-match "PRIMARY SELECTION" prompt))
                   (support-start
                    (string-match "SUPPORTING CURRENT THREAD" prompt))
                   (selection (substring prompt selection-start support-start))
                   (supporting (substring prompt support-start)))
              (should selection-start)
              (should support-start)
              (should (string-match-p
                       (regexp-quote "SELECTED region\n") selection))
              (should-not (string-match-p
                           "displayed before\\|displayed after" selection))
              (should (string-match-p
                       (regexp-quote "FULL MESSAGE") supporting))
              (should (string-match-p
                       (regexp-quote "COLLAPSED MESSAGE") supporting))
              (should (string-match-p "SECONDARY CONTEXT" supporting))
              (should (string-match-p
                       (regexp-quote "not thread:thread-1") supporting)))
            (should
             (member
              '("search" "--output=threads" "--format=text" "--limit=2"
                "--exclude=false" "--" "id:message-1")
              notmuch-calls))
            (should
             (member
              '("show" "--format=text" "--entire-thread=true"
                "--exclude=false" "--" "thread:thread-1")
              notmuch-calls)))
        (should (equal (buffer-string) before-text)))))))

(ert-deftest envoy-test-notmuch-prefix-region-keeps-thread-out-of-task ()
  "With a prefix and a region, the task covers the selection alone.
The scope comes before any quoted mail, and the complete thread is marked as
reference the agent must not act on."
  (with-temp-buffer
    (insert "Finding 1. Fix the guard\nFinding 9. Remember CI runs\n")
    (let ((major-mode 'notmuch-show-mode)
          (transient-mark-mode t)
          (sent nil))
      (goto-char (point-min))
      (search-forward "Finding 9")
      (set-mark (line-beginning-position))
      (goto-char (line-end-position))
      (activate-mark)
      (envoy-test--with-notmuch-thread
          "id:message-9" "thread:thread-9"
          "Finding 1. Fix the guard\nFinding 9. Remember CI runs\n"
        (cl-letf (((symbol-function 'envoy-tmux--read-variable)
                   (lambda (provider)
                     (should (eq provider 'omp))
                     (list :model "planner" :plan-into "executor")))
                  ;; No agent is installed where this suite runs, so the
                  ;; dispatch resolves this stand-in instead.  Nothing here
                  ;; starts it: `envoy-tmux-send' below records the brief.
                  ((symbol-function 'envoy-tmux--omp-program)
                   (lambda () "true"))
                  ((symbol-function 'envoy--read-direction)
                   (lambda (&rest _) "Improve this"))
                  ((symbol-function 'envoy-tmux-read-session)
                   (lambda (&rest _) "work"))
                  ((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "work:9"))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (target prompt &rest _)
                     (setq sent (list target prompt))))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (envoy-notmuch-delegate '(4))
          (let* ((prompt (nth 1 sent))
                 (instruction-start (string-match "^DIRECTION FROM THE USER$" prompt))
                 (scope-start (string-match "^TASK SCOPE$" prompt))
                 (first-quote
                  (string-match
                   (regexp-quote "--- BEGIN UNTRUSTED QUOTED MAIL ---") prompt))
                 (support-start
                  (string-match "^SUPPORTING CURRENT THREAD$" prompt))
                 (thread-start (string-match "^CURRENT THREAD$" prompt)))
            (should scope-start)
            (should (< instruction-start scope-start first-quote))
            (let ((scope (substring prompt scope-start first-quote)))
              (should (string-match-p "PRIMARY SELECTION" scope))
              (should (string-match-p "outside the selection" scope)))
            (should (< support-start thread-start))
            (should (string-match-p
                     "Do not act on"
                     (substring prompt support-start thread-start)))))))))

(ert-deftest envoy-test-notmuch-brief-routes-authorization-gates-through-ask ()
  "A mail brief tells the agent to ask for approval instead of parking a change."
  (let ((prompt (envoy-notmuch--prompt
                 "Improve this" "id:m" "thread:t" "Body" "Body" nil)))
    (should (string-match-p "^APPROVALS$" prompt))
    (should (string-match-p "`ask` tool" prompt))
    (should (string-match-p "Never write a separate handoff" prompt))
    (should (< (string-match "^APPROVALS$" prompt)
               (string-match "^EMAIL SAFETY$" prompt)))))

(ert-deftest envoy-test-org-brief-guard-names-the-user-as-dispatcher ()
  "The outward-action guard says the user dispatched the brief."
  (envoy-test--with-org "* TODO The task\nDo the thing.\n"
    (org-back-to-heading t)
    (let ((prompt (envoy-org-build-prompt
                   "The task" (envoy-org--subtree-text) "/tmp/x" nil)))
      (should (string-match-p "The user dispatched this Org brief" prompt))
      (should-not (string-match-p "No person typed" prompt)))))

(ert-deftest envoy-test-notmuch-no-region-uses-complete-thread-query ()
  "Without a region, the prompt uses the CLI's complete thread text.
Text visible in a filtered show buffer is not treated as the thread."
  (with-temp-buffer
    (insert "FILTERED BUFFER MESSAGE\n")
    (let ((major-mode 'notmuch-show-mode)
          (transient-mark-mode nil)
          sent)
      (envoy-test--with-notmuch-thread
          "id:message-2" "thread:thread-2"
          "COMPLETE MESSAGE ONE\nCOLLAPSED MESSAGE TWO\n"
        (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                   (lambda () "/fake/omp"))
                  ((symbol-function 'envoy--read-direction)
                   (lambda (&rest _) "Summarize the thread"))
                  ((symbol-function 'envoy-tmux-read-session)
                   (lambda (&rest _) "work"))
                  ((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "work:8"))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (target prompt &optional provider model setup
                                    plan-into &rest _)
                     (setq sent (list target prompt provider model setup
                                      plan-into))))
                  ((symbol-function 'message) (lambda (&rest _) nil)))
          (envoy-notmuch-delegate)
          (let ((prompt (nth 1 sent)))
            (should (string-match-p
                     (regexp-quote "COMPLETE MESSAGE ONE") prompt))
            (should (string-match-p
                     (regexp-quote "COLLAPSED MESSAGE TWO") prompt))
            (should-not (string-match-p
                         (regexp-quote "FILTERED BUFFER MESSAGE") prompt)))
          (should
           (member
            '("search" "--output=threads" "--format=text" "--limit=2"
              "--exclude=false" "--" "id:message-2")
            notmuch-calls))
          (should
           (member
            '("show" "--format=text" "--entire-thread=true"
              "--exclude=false" "--" "thread:thread-2")
            notmuch-calls)))))))

(ert-deftest envoy-test-notmuch-preflight-rejects-mode-and-identity ()
  "A list/tree buffer and a show buffer without message identity do no work."
  (dolist (mode '(notmuch-tree-mode notmuch-search-mode fundamental-mode))
    (with-temp-buffer
      (let ((major-mode mode)
            (window-called nil)
            (send-called nil)
            (cli-called nil))
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) (setq window-called t)))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (&rest _) (setq send-called t)))
                  ((symbol-function 'notmuch-command-to-string)
                   (lambda (&rest _) (setq cli-called t)))
                  ((symbol-function 'envoy-tmux--omp-program)
                   (lambda () (error "OMP must not be checked")))
                  ((symbol-function 'envoy--read-direction)
                   (lambda (&rest _) (error "Instruction must not be read")))
                  ((symbol-function 'require)
                   (lambda (&rest _) t)))
          (should-error (envoy-notmuch-delegate) :type 'user-error))
        (should-not window-called)
        (should-not send-called)
        (should-not cli-called))))
  (with-temp-buffer
    (let ((major-mode 'notmuch-show-mode)
          (window-called nil)
          (send-called nil)
          (cli-called nil))
      (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'notmuch-show-get-message-id)
                 (lambda (&optional _) nil))
                ((symbol-function 'envoy-tmux-window)
                 (lambda (&rest _) (setq window-called t)))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (&rest _) (setq send-called t)))
                ((symbol-function 'notmuch-command-to-string)
                 (lambda (&rest _) (setq cli-called t)))
                ((symbol-function 'envoy--read-direction)
                 (lambda (&rest _) (error "Instruction must not be read"))))
        (should-error (envoy-notmuch-delegate) :type 'user-error))
      (should-not window-called)
      (should-not send-called)
      (should-not cli-called))))

(ert-deftest envoy-test-notmuch-empty-direction-uses-default ()
  "A blank direction uses the default and the command remains interactive."
  (should (commandp #'envoy-notmuch-delegate))
  (with-temp-buffer
    (let ((major-mode 'notmuch-show-mode)
          (sent nil)
          (window-called nil))
      (envoy-test--with-notmuch-thread
          "id:message-3" "thread:thread-3" "THREAD\n"
        (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                   (lambda () "/fake/omp"))
                  ((symbol-function 'read-string)
                   (lambda (prompt)
                     (should (equal prompt "Direction for OMP: "))
                     "   "))
                  ((symbol-function 'envoy-tmux-read-session)
                   (lambda (&rest _) "work"))
                  ((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _)
                     (setq window-called t)
                     "work:7"))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (_target prompt &rest _)
                     (setq sent prompt)))
                  ((symbol-function 'require) (lambda (&rest _) t)))
          (should (equal (envoy-notmuch-delegate) "work:7"))
          (should window-called)
          (should (string-match-p
                   (regexp-quote
                    (concat "DIRECTION FROM THE USER\n"
                            envoy-default-direction "\n\n"))
                   sent)))))))

(ert-deftest envoy-test-notmuch-session-cancellation-stops-before-dispatch ()
  "Cancelling the session picker leaves tmux untouched."
  (with-temp-buffer
    (let ((major-mode 'notmuch-show-mode)
          (window-called nil)
          (send-called nil)
          (write-called nil))
      (envoy-test--with-notmuch-thread
          "id:message-4" "thread:thread-4" "THREAD\n"
        (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                   (lambda () "/fake/omp"))
                  ((symbol-function 'envoy--read-direction)
                   (lambda (&rest _) "Continue"))
                  ((symbol-function 'envoy-tmux-read-session)
                   (lambda (&rest _) (signal 'quit nil)))
                  ((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) (setq window-called t)))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (&rest _) (setq send-called t)))
                  ((symbol-function 'envoy-tmux--write-prompt)
                   (lambda (&rest _) (setq write-called t)))
                  ((symbol-function 'require) (lambda (&rest _) t)))
          (condition-case nil
              (envoy-notmuch-delegate)
            (quit nil)))
        (should-not window-called)
        (should-not send-called)
        (should-not write-called)))))

(ert-deftest envoy-test-notmuch-cli-failure-stops-before-dispatch ()
  "A notmuch CLI error never reaches tmux."
  (with-temp-buffer
    (let ((major-mode 'notmuch-show-mode)
          (window-called nil)
          (send-called nil))
      (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'notmuch-show-get-message-id)
                 (lambda (&optional _) "id:message-5"))
                ((symbol-function 'notmuch-command-to-string)
                 (lambda (&rest _) (error "Notmuch failed")))
                ((symbol-function 'envoy-tmux--omp-program)
                 (lambda () (error "OMP must not be checked")))
                ((symbol-function 'envoy-tmux-window)
                 (lambda (&rest _) (setq window-called t)))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (&rest _) (setq send-called t)))
                ((symbol-function 'envoy--read-direction)
                 (lambda (&rest _) (error "Instruction must not be read"))))
        (should-error (envoy-notmuch-delegate) :type 'user-error))
      (should-not window-called)
      (should-not send-called))))

(ert-deftest envoy-test-notmuch-empty-thread-stops-before-dispatch ()
  "An empty complete-thread response never reaches tmux."
  (with-temp-buffer
    (let ((major-mode 'notmuch-show-mode)
          (window-called nil)
          (send-called nil))
      (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'notmuch-show-get-message-id)
                 (lambda (&optional _) "id:message-empty"))
                ((symbol-function 'notmuch-command-to-string)
                 (lambda (&rest args)
                   (if (equal (car args) "search")
                       "thread:empty\n"
                     "")))
                ((symbol-function 'envoy-tmux--omp-program)
                 (lambda () (error "OMP must not be checked")))
                ((symbol-function 'envoy-tmux-window)
                 (lambda (&rest _) (setq window-called t)))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (&rest _) (setq send-called t)))
                ((symbol-function 'envoy--read-direction)
                 (lambda (&rest _) (error "Instruction must not be read"))))
        (should-error (envoy-notmuch-delegate) :type 'user-error))
      (should-not window-called)
      (should-not send-called))))

(ert-deftest envoy-test-notmuch-ambiguous-thread-query-stops-before-dispatch ()
  "More than one resolved thread is rejected before tmux dispatch."
  (with-temp-buffer
    (let ((major-mode 'notmuch-show-mode)
          (window-called nil)
          (send-called nil))
      (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                ((symbol-function 'notmuch-show-get-message-id)
                 (lambda (&optional _) "id:message-6"))
                ((symbol-function 'notmuch-command-to-string)
                 (lambda (&rest args)
                   (if (equal (car args) "search")
                       "thread:one\nthread:two\n"
                     (error "Show must not run for an ambiguous query"))))
                ((symbol-function 'envoy-tmux--omp-program)
                 (lambda () (error "OMP must not be checked")))
                ((symbol-function 'envoy-tmux-window)
                 (lambda (&rest _) (setq window-called t)))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (&rest _) (setq send-called t)))
                ((symbol-function 'envoy--read-direction)
                 (lambda (&rest _) (error "Instruction must not be read"))))
        (should-error (envoy-notmuch-delegate) :type 'user-error))
      (should-not window-called)
      (should-not send-called))))

(ert-deftest envoy-test-notmuch-prefix-single-model-reaches-omp-command ()
  "A prefix-selected single OMP model reaches command construction."
  (let (prompt-file)
    (unwind-protect
        (envoy-test--with-tmux '("work")
          (with-temp-buffer
            (insert "MESSAGE\n")
            (let ((major-mode 'notmuch-show-mode)
                  (envoy-tmux-providers
                   '((omp :program "true" :name "OMP" :args ()
                          :prompt-arg "@%s" :model-arg "--model"
                          :variable model :setup nil :models ("single")))))
              (envoy-test--with-notmuch-thread
                  "id:message-7" "thread:thread-7" "THREAD\n"
                (cl-letf (((symbol-function 'envoy--read-direction)
                           (lambda (&rest _) "Run it"))
                          ((symbol-function 'envoy-tmux-read-session)
                           (lambda (&rest _) "work"))
                          ((symbol-function 'completing-read)
                           (lambda (prompt &rest _)
                             (if (string-match-p "launch mode" prompt)
                                 "Single model"
                               "single")))
                          ((symbol-function 'envoy-tmux--write-prompt)
                           (lambda (prompt)
                             (setq prompt-file
                                   (make-temp-file "envoy-test-notmuch-prompt-"))
                             (with-temp-file prompt-file (insert prompt))
                             prompt-file))
                          ((symbol-function 'message) (lambda (&rest _) nil)))
                  (envoy-notmuch-delegate '(4))))
              (let ((send (seq-find
                           (lambda (call)
                             (and (equal (car call) "send-keys")
                                  (member "-l" call)))
                           (envoy-test--tmux-calls))))
                (should send)
                (should (string-match-p "--model single" (nth 4 send)))
                (should-not (string-match-p "--plan-yolo" (nth 4 send)))))))
      (when (and prompt-file (file-exists-p prompt-file))
        (delete-file prompt-file)))))

(ert-deftest envoy-test-notmuch-prefix-plan-into-reaches-omp-command ()
  "A prefix-selected planner and executor reach OMP plan command flags."
  (let (prompt-file)
    (unwind-protect
        (envoy-test--with-tmux '("work")
          (with-temp-buffer
            (insert "MESSAGE\n")
            (let ((major-mode 'notmuch-show-mode)
                  (envoy-tmux-providers
                   '((omp :program "true" :name "OMP" :args ()
                          :prompt-arg "@%s" :model-arg "--model"
                          :variable model :setup nil
                          :models ("planner" "executor")))))
              (envoy-test--with-notmuch-thread
                  "id:message-8" "thread:thread-8" "THREAD\n"
                (cl-letf (((symbol-function 'envoy--read-direction)
                           (lambda (&rest _) "Plan it"))
                          ((symbol-function 'envoy-tmux-read-session)
                           (lambda (&rest _) "work"))
                          ((symbol-function 'completing-read)
                           (lambda (prompt &rest _)
                             (cond
                              ((string-match-p "launch mode" prompt) "Plan into")
                              ((string-match-p "Planner" prompt) "planner")
                              (t "executor"))))
                          ((symbol-function 'envoy-tmux--write-prompt)
                           (lambda (prompt)
                             (setq prompt-file
                                   (make-temp-file "envoy-test-notmuch-prompt-"))
                             (with-temp-file prompt-file (insert prompt))
                             prompt-file))
                          ((symbol-function 'message) (lambda (&rest _) nil)))
                  (envoy-notmuch-delegate '(4))))
              (let ((send (seq-find
                           (lambda (call)
                             (and (equal (car call) "send-keys")
                                  (member "-l" call)))
                           (envoy-test--tmux-calls))))
                (should send)
                (should (string-match-p
                         "--model planner.*--plan-yolo.*--plan-yolo-into executor"
                         (nth 4 send)))))))
      (when (and prompt-file (file-exists-p prompt-file))
        (delete-file prompt-file)))))

(ert-deftest envoy-test-org-terminal-duplicate-claim-stops-before-window ()
  "An active claim or pending marker blocks terminal dispatch before tmux."
  (let ((spool (make-temp-file "envoy-test-claims-" t))
        (window-calls 0)
        (send-calls 0))
    (unwind-protect
        (let ((envoy-org-spool-directory spool)
              (envoy-tmux-providers
               '((claude :program "true" :name "Claude"))))
          (envoy-test--with-org-file
              "#+TODO: TODO DOING | DONE\n* TODO The task\n"
            (let* ((identity (envoy-org--heading-identity))
                   (id (nth 0 identity))
                   (key (envoy-org--spool-key id))
                   (claim (envoy-org--spool-claim id "The task")))
              (when (buffer-modified-p) (save-buffer))
              (cl-letf (((symbol-function 'envoy-tmux-window)
                         (lambda (&rest _)
                           (setq window-calls (1+ window-calls))))
                        ((symbol-function 'envoy-tmux-send)
                         (lambda (&rest _)
                           (setq send-calls (1+ send-calls))))
                        ((symbol-function 'message) (lambda (&rest _) nil)))
                (should-error
                 (envoy-tmux-org-heading-to 'claude nil nil nil "test")
                 :type 'user-error))
              (envoy-org--spool-release-claim claim)
              (dolist (suffix (list envoy-org--spool-done-suffix
                                    envoy-org--spool-failed-suffix
                                    envoy-org--spool-state-suffix))
                (let ((pending (envoy-org--spool-file key suffix)))
                  (unwind-protect
                      (progn
                        (with-temp-file pending (insert "pending\n"))
                        (should-error
                         (envoy-tmux-org-heading-to
                          'claude nil nil nil "test")
                         :type 'user-error))
                    (when (file-exists-p pending)
                      (delete-file pending))))))
            (should (= window-calls 0))
            (should (= send-calls 0))))
      (delete-directory spool t))))

(ert-deftest envoy-test-org-terminal-setup-failure-releases-claim ()
  "A tmux setup failure leaves no active claim or terminal state."
  (let ((spool (make-temp-file "envoy-test-claim-failure-" t)))
    (unwind-protect
        (let ((envoy-org-spool-directory spool)
              (envoy-tmux-providers
               '((claude :program "true" :name "Claude"))))
          (envoy-test--with-org-file
              "#+TODO: TODO DOING | DONE\n* TODO The task\n"
            (cl-letf (((symbol-function 'envoy-tmux-window)
                       (lambda (&rest _)
                         (user-error "Window failed"))))
              (should-error
               (envoy-tmux-org-heading-to 'claude nil nil nil "test")
               :type 'user-error))
            (let* ((id (org-entry-get nil "ID"))
                   (key (envoy-org--spool-key id)))
              (should-not
               (file-exists-p
                (envoy-org--spool-file key envoy-org--spool-claim-suffix)))
              (should-not
               (file-exists-p
                (envoy-org--spool-file key envoy-org--spool-state-suffix))))))
      (delete-directory spool t))))

(ert-deftest envoy-test-org-nonterminal-claim-releases-after-callback ()
  "A non-terminal callback releases its heading claim."
  (let ((spool (make-temp-file "envoy-test-nonterminal-claim-" t))
        callback
        (run-calls 0))
    (unwind-protect
        (let ((envoy-org-spool-directory spool)
              (envoy-provider 'test)
              (envoy-providers
               '((test :program "true" :name "test agent"
                       :args nil :edit-args nil
                       :deny-arg nil :model-arg nil)))
              (envoy-org-output-instructions nil)
              (envoy-org-outward-action-guard nil))
          (envoy-test--with-org-file
              "#+TODO: TODO DOING | DONE\n* TODO The task\n"
            (cl-letf (((symbol-function 'envoy-org--attach-directory)
                       (lambda () nil))
                      ((symbol-function 'envoy-run)
                       (lambda (_prompt cb &rest _keys)
                         (setq callback cb)
                         (setq run-calls (1+ run-calls))
                         'fake-process))
                      ((symbol-function 'message) (lambda (&rest _) nil)))
              (envoy-org-heading))
            (let* ((id (org-entry-get nil "ID"))
                   (key (envoy-org--spool-key id))
                   (claim-file
                    (envoy-org--spool-file
                     key envoy-org--spool-claim-suffix)))
              (should (file-exists-p claim-file))
              (set-buffer-modified-p nil)
              (should-error (envoy-org-heading) :type 'user-error)
              (should (= run-calls 1))
              (funcall callback
                       (envoy--make-result
                        :ok t :text "ENVOY_STATUS: complete\nDone." :status 0))
              (should-not (file-exists-p claim-file)))))
      (delete-directory spool t))))

(ert-deftest envoy-test-org-terminal-claim-lasts-until-report-collection ()
  "A successful terminal claim remains until a durable report collection."
  (let ((spool (make-temp-file "envoy-test-terminal-claim-" t)))
    (unwind-protect
        (let ((envoy-org-spool-directory spool)
              (envoy-tmux-providers
               '((claude :program "true" :name "Claude"))))
          (envoy-test--with-org-file
              "#+TODO: TODO DOING | DONE\n* TODO The task\nBody.\n"
            (cl-letf (((symbol-function 'envoy-tmux-window)
                       (lambda (&rest _) "test:7"))
                      ((symbol-function 'envoy-tmux-send)
                       (lambda (&rest _) nil))
                      ((symbol-function 'envoy-org--attach-directory)
                       (lambda () nil))
                      ((symbol-function 'message) (lambda (&rest _) nil)))
              (envoy-tmux-org-heading-to 'claude nil nil nil "test"))
            (let* ((id (org-entry-get nil "ID"))
                   (key (envoy-org--spool-key id))
                   (claim (envoy-org--spool-file
                           key envoy-org--spool-claim-suffix))
                   (state (envoy-org--spool-file
                           key envoy-org--spool-state-suffix))
                   (done (envoy-org--spool-file
                          key envoy-org--spool-done-suffix)))
              (should (file-exists-p claim))
              (should (file-exists-p state))
              (envoy-test--publish-spool-marker
               done "The report is complete.\n")
              (should (= (envoy-org-collect-reports) 1))
              (should-not (file-exists-p claim))
              (should-not (file-exists-p state))
              (should-not (file-exists-p done))
              (with-temp-buffer
                (insert-file-contents file)
                (should (string-match-p "^\\* DONE The task" (buffer-string)))
                (should (string-match-p "The report is complete\\."
                                        (buffer-string)))))))
      (delete-directory spool t))))

(ert-deftest envoy-test-org-collection-retains-marker-when-save-fails ()
  "A failed heading save retains the marker and state for retry."
  (let ((spool (make-temp-file "envoy-test-collection-save-" t)))
    (unwind-protect
        (let ((envoy-org-spool-directory spool))
          (envoy-test--with-org-file "* TODO The task\n"
            (let* ((key (envoy-org--spool-open "The task" nil))
                   (state (envoy-org--spool-file
                           key envoy-org--spool-state-suffix))
                   (done (envoy-org--spool-file
                          key envoy-org--spool-done-suffix)))
              (envoy-test--publish-spool-marker done "The report.\n")
              (cl-letf (((symbol-function 'save-buffer)
                         (lambda (&rest _)
                           (error "Save failed"))))
                (should-error (envoy-org-collect-reports)))
              (should (file-exists-p done))
              (should (file-exists-p state)))))
      (delete-directory spool t))))

(defun envoy-test--publish-spool-marker (file text &optional status)
  "Publish TEXT and STATUS to FILE through a same-directory rename."
  (let* ((status (or status
                     (cond
                      ((string-suffix-p envoy-org--spool-done-suffix file)
                       "complete")
                      ((string-suffix-p envoy-org--spool-failed-suffix file)
                       "incomplete"))))
         (temporary
          (make-temp-file
           (expand-file-name ".envoy-test-marker-"
                             (file-name-directory file)))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (when status
              (insert (format "ENVOY_STATUS: %s\n" status)))
            (insert text))
          (rename-file temporary file t))
      (when (file-exists-p temporary)
        (delete-file temporary)))))

(ert-deftest envoy-test-org-quiet-collector-is-silent-but-manual-report-is-visible ()
  "Background collection stays silent while a manual sweep remains visible."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO Other task\nUnchanged.\n* TODO The task\nBody.\n"
    (let* ((messages (get-buffer-create "*Messages*"))
           (original-messages
            (with-current-buffer messages (buffer-string)))
           (original-message-point
            (with-current-buffer messages (point)))
           (original-messages-modified
            (with-current-buffer messages (buffer-modified-p)))
           (original-messages-read-only
            (with-current-buffer messages buffer-read-only))
           (message-log-max t))
      (unwind-protect
          (progn
            (let ((before (with-current-buffer messages (buffer-string))))
              (envoy-org--collect-quietly)
              (with-current-buffer messages
                (should (equal before (buffer-string)))))
            (let* ((missing-id "quiet-collector-missing-heading")
                   (missing-key (envoy-org--spool-key missing-id))
                   (missing-claim
                    (envoy-org--spool-claim missing-id "Missing heading"))
                   (missing-state
                    (envoy-org--spool-write-state missing-key missing-id nil))
                   (missing-done
                    (envoy-org--spool-file
                     missing-key envoy-org--spool-done-suffix)))
              (envoy-test--publish-spool-marker
               missing-done "Waiting report.\n")
              (re-search-forward "^\\* TODO The task$")
              (org-back-to-heading t)
              (let* ((target-id (org-id-get-create))
                     (target-claim
                      (envoy-org--spool-claim target-id "The task"))
                     (target-key (envoy-org--spool-open "The task" nil))
                     (target-state
                      (envoy-org--spool-file
                       target-key envoy-org--spool-state-suffix))
                     (target-done
                      (envoy-org--spool-file
                       target-key envoy-org--spool-done-suffix))
                     (messages-before
                      (with-current-buffer messages (buffer-string))))
                (envoy-test--publish-spool-marker
                 target-done "The filed report.\n")
                (dotimes (_ 2)
                  (envoy-org--collect-quietly)
                  (with-current-buffer messages
                    (should (equal messages-before (buffer-string)))))
                (should (file-exists-p (plist-get missing-claim :file)))
                (should (file-exists-p missing-state))
                (should (equal "ENVOY_STATUS: complete\nWaiting report.\n"
                               (envoy-test--file-text missing-done)))
                (should-not (file-exists-p (plist-get target-claim :file)))
                (should-not (file-exists-p target-state))
                (should-not (file-exists-p target-done))
                (let* ((saved (envoy-test--file-text file))
                       (other (string-match "^\\* TODO Other task" saved))
                       (target (string-match "^\\* DONE The task" saved))
                       (report (string-match "The filed report\\." saved)))
                  (should other)
                  (should target)
                  (should report)
                  (should (< other target))
                  (should (< target report))
                  (should-not
                   (string-match-p
                    "The filed report\\." (substring saved other target))))
                (let ((manual-before
                       (with-current-buffer messages (buffer-string))))
                  (let ((inhibit-message nil))
                    (should (= (envoy-org-collect-reports) 0)))
                  (with-current-buffer messages
                    (let ((after (buffer-string)))
                      (should (string-prefix-p manual-before after))
                      (should
                       (string-match-p
                        "Envoy:" (substring after (length manual-before)))))))
                (should (file-exists-p missing-done)))))
        (with-current-buffer messages
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert original-messages)
            (goto-char (min original-message-point (point-max)))
            (set-buffer-modified-p original-messages-modified)
            (setq buffer-read-only original-messages-read-only)))))))

(ert-deftest envoy-test-tmux-prefixed-wrapper-selects-after-claim ()
  "A prefixed public wrapper selects OMP only after it claims the heading."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n:PROPERTIES:\n:ID: prefixed-wrapper\n:END:\n"
    (let ((envoy-tmux-provider 'omp)
          (envoy-tmux-providers
           '((omp :program "true" :name "OMP" :args ()
                  :prompt-arg "@%s" :model-arg "--model"
                  :variable model :setup nil :models ("configured"))))
          (selection-called nil)
          (selection-saw-claim nil)
          (window-called nil)
          sent-model)
      (let* ((key (envoy-org--spool-key (org-entry-get nil "ID")))
             (claim-file
              (envoy-org--spool-file key envoy-org--spool-claim-suffix))
             (state-file
              (envoy-org--spool-file key envoy-org--spool-state-suffix)))
        (cl-letf (((symbol-function 'envoy-tmux--read-variable)
                   (lambda (&rest _)
                     (setq selection-called t
                           selection-saw-claim (file-exists-p claim-file))
                     '(:model "selected-model" :setup nil :plan-into nil)))
                  ((symbol-function 'envoy-tmux-read-session)
                   (lambda (&rest _) "test"))
                  ((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _)
                     (setq window-called t)
                     "test:7"))
                  ((symbol-function 'envoy-tmux-send)
                   (lambda (_target _prompt &optional _provider model
                                     _setup _plan-into on-start _claim &rest _)
                     (setq sent-model model)
                     (when on-start (funcall on-start))))
                  ((symbol-function 'envoy-org--attach-directory)
                   (lambda () nil))
                  ((symbol-function 'message)
                   (lambda (&rest _) nil)))
          (envoy-tmux-org-heading-omp '(4)))
        (should selection-called)
        (should selection-saw-claim)
        (should window-called)
        (should (equal sent-model "selected-model"))
        (should (file-exists-p claim-file))
        (should (file-exists-p state-file))))))

(ert-deftest envoy-test-tmux-default-omp-rejection-follows-claim-before-discovery ()
  "Default OMP rejection occurs after claim and before discovery or tmux."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n:PROPERTIES:\n:ID: default-omp\n:END:\n"
    (let ((envoy-tmux-provider 'omp)
          (envoy-tmux-providers
           '((omp :program "true" :name "OMP" :args ()
                  :prompt-arg "@%s" :model-arg "--model"
                  :variable model :setup nil :models nil)))
          (claim-seen nil)
          (direction-called nil)
          (selector-called nil)
          (discovery-called nil)
          (window-called nil)
          (session-called nil))
      (let* ((key (envoy-org--spool-key (org-entry-get nil "ID")))
             (claim-file
              (envoy-org--spool-file key envoy-org--spool-claim-suffix)))
        (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                   (lambda ()
                     (setq claim-seen (file-exists-p claim-file))
                     (user-error "OMP is unavailable")))
                  ((symbol-function 'envoy--read-direction)
                   (lambda (&rest _)
                     (setq direction-called t)
                     (error "Direction must not be read")))
                  ((symbol-function 'envoy-tmux--read-variable)
                   (lambda (&rest _)
                     (setq selector-called t)
                     (error "Selector must not run")))
                  ((symbol-function 'envoy-tmux--omp-models)
                   (lambda (&rest _)
                     (setq discovery-called t)
                     (error "Discovery must not run")))
                  ((symbol-function 'envoy-tmux-read-session)
                   (lambda (&rest _)
                     (setq session-called t)
                     (error "Session selection must not run")))
                  ((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _)
                     (setq window-called t)
                     (error "Window must not run"))))
          (should-error (envoy-tmux-org-heading-omp) :type 'user-error))
        (should claim-seen)
        (should-not direction-called)
        (should-not selector-called)
        (should-not discovery-called)
        (should-not session-called)
        (should-not window-called)
        (should-not (file-exists-p claim-file))))))

(ert-deftest envoy-test-org-report-off-still-blocks-duplicate-and-collects ()
  "Report prose off still claims terminal work and permits collection."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n:PROPERTIES:\n:ID: report-off\n:END:\n"
    (let ((envoy-org-report-instructions nil)
          (envoy-tmux-providers
           '((claude :program "true" :name "Claude" :args ()
                     :prompt-arg nil :variable setup :setup nil :setups ())))
          (window-calls 0))
      (cl-letf (((symbol-function 'envoy-tmux-window)
                 (lambda (&rest _)
                   (setq window-calls (1+ window-calls))
                   "test:7"))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (&rest _) nil))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (&rest _) nil)))
        (envoy-tmux-org-heading-to 'claude nil nil nil "test")
        (when (buffer-modified-p)
          (save-buffer))
        (let* ((key (envoy-org--spool-key (org-entry-get nil "ID")))
               (claim (envoy-org--spool-file
                       key envoy-org--spool-claim-suffix))
               (state (envoy-org--spool-file
                       key envoy-org--spool-state-suffix))
               (done (envoy-org--spool-file
                      key envoy-org--spool-done-suffix)))
          (should (file-exists-p claim))
          (should (file-exists-p state))
          (should-error
           (envoy-tmux-org-heading-to 'claude nil nil nil "test")
           :type 'user-error)
          (should (= window-calls 1))
          (envoy-test--publish-spool-marker done "The report is complete.\n")
          (should (= (envoy-org-collect-reports) 1))
          (should-not (file-exists-p claim))
          (should-not (file-exists-p state))
          (should-not (file-exists-p done))
          (should (equal (org-get-todo-state) "DONE"))
          (should (string-match-p "The report is complete\."
                                  (buffer-string))))))))

(ert-deftest envoy-test-org-collector-preserves-coexisting-markers ()
  "Done and failed markers for one claim are left untouched."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n:PROPERTIES:\n:ID: marker-conflict\n:END:\n"
    (let* ((id (org-entry-get nil "ID"))
           (key (envoy-org--spool-key id))
           (claim (envoy-org--spool-claim id "The task"))
           (state (envoy-org--spool-file
                   key envoy-org--spool-state-suffix))
           (done (envoy-org--spool-file
                  key envoy-org--spool-done-suffix))
           (failed (envoy-org--spool-file
                    key envoy-org--spool-failed-suffix))
           (before (buffer-string)))
      (envoy-org--spool-open "The task" nil)
      (envoy-test--publish-spool-marker done "done report\n")
      (envoy-test--publish-spool-marker failed "failed report\n")
      (should (= (envoy-org-collect-reports) 0))
      (should (file-exists-p (plist-get claim :file)))
      (should (file-exists-p state))
      (should (file-exists-p done))
      (should (file-exists-p failed))
      (should (equal before (buffer-string)))
      (should-not (string-match-p ":ENVOY:" (buffer-string))))))

(ert-deftest envoy-test-org-collector-completion-during-send-stays-done ()
  "A report collected during send cannot be reset to the active state."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO The task\n:PROPERTIES:\n:ID: collect-during-send\n:END:\n"
    (let ((envoy-tmux-providers
           '((claude :program "true" :name "Claude" :args ()
                     :prompt-arg nil :variable setup :setup nil :setups ())))
          collected)
      (cl-letf (((symbol-function 'envoy-tmux-window)
                 (lambda (&rest _) "test:7"))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (_target _prompt &optional _provider _model _setup
                                   _plan-into on-start _claim &rest _)
                   (when on-start (funcall on-start))
                   (let ((done (envoy-org--spool-file
                                (envoy-org--spool-key
                                 (org-entry-get nil "ID"))
                                envoy-org--spool-done-suffix)))
                     (envoy-test--publish-spool-marker
                      done "The report is complete.\n")
                     (setq collected (= (envoy-org-collect-reports) 1)))))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (&rest _) nil)))
        (should (equal (envoy-tmux-org-heading-to
                        'claude nil nil nil "test")
                       "test:7")))
      (should collected)
      (should (equal (org-get-todo-state) "DONE"))
      (should-not (equal (org-get-todo-state) "DOING"))
      (should (string-match-p "The report is complete\."
                              (buffer-string))))))


(defun envoy-test--file-text (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(ert-deftest envoy-test-org-repeating-collection-save-failure-rolls-back-and-retries-once ()
  "A failed save rolls back a repeat and preserves dirty user text for retry."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO Repeat\nSCHEDULED: <2026-09-19 Sat +1d>\n:PROPERTIES:\n:ID: repeating-save\n:END:\n"
    (let* ((id (org-entry-get nil "ID"))
           (key (envoy-org--spool-key id))
           (claim (envoy-org--spool-claim id "Repeat"))
           (state (envoy-org--spool-file
                   key envoy-org--spool-state-suffix))
           (done (envoy-org--spool-file
                  key envoy-org--spool-done-suffix))
           (schedule-before (org-entry-get nil "SCHEDULED")))
      (envoy-org--spool-open "Repeat" nil)
      (envoy-test--publish-spool-marker done "Retry report.\n")
      (goto-char (point-max))
      (insert "User text survives the failed save.\n")
      (let ((before-text (buffer-string)))
        (cl-letf (((symbol-function 'save-buffer)
                   (lambda (&rest _)
                     (error "Save failed"))))
          (should-error (envoy-org-collect-reports)))
        (should (buffer-modified-p))
        (should (equal before-text (buffer-string)))
        (should (equal schedule-before (org-entry-get nil "SCHEDULED")))
        (should (file-exists-p (plist-get claim :file)))
        (should (file-exists-p state))
        (should (file-exists-p done))
        (should-not (string-match-p "User text survives"
                                    (envoy-test--file-text file)))
        (should (string-match-p "^\\* TODO Repeat"
                                (envoy-test--file-text file)))
        (should (= (envoy-org-collect-reports) 1))
        (should-not (file-exists-p (plist-get claim :file)))
        (should-not (file-exists-p state))
        (should-not (file-exists-p done))
        (should (equal (org-get-todo-state) "TODO"))
        (should (equal "<2026-09-20 Sun +1d>"
                       (org-entry-get nil "SCHEDULED")))
        (should (string-match-p "User text survives"
                                (buffer-string)))
        (should (= 1 (how-many "Retry report\\." (point-min) (point-max))))
        (let ((schedule-after (org-entry-get nil "SCHEDULED")))
          (should (= (envoy-org-collect-reports) 0))
          (should (equal schedule-after (org-entry-get nil "SCHEDULED"))))))))

(ert-deftest envoy-test-org-collection-rejects-dirty-save-with-evidence-intact ()
  "A save that returns with a dirty buffer keeps the report for retry."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO Dirty save\n:PROPERTIES:\n:ID: dirty-save\n:END:\n"
    (let* ((id (org-entry-get nil "ID"))
           (key (envoy-org--spool-key id))
           (claim (envoy-org--spool-claim id "Dirty save"))
           (state (envoy-org--spool-file
                   key envoy-org--spool-state-suffix))
           (done (envoy-org--spool-file
                  key envoy-org--spool-done-suffix))
           (before (buffer-string)))
      (envoy-org--spool-open "Dirty save" nil)
      (envoy-test--publish-spool-marker done "Dirty report.\n")
      (cl-letf (((symbol-function 'save-buffer)
                 (lambda (&rest _)
                   (set-buffer-modified-p t))))
        (should-error (envoy-org-collect-reports)))
      (should-not (buffer-modified-p))
      (should (equal before (buffer-string)))
      (should (file-exists-p (plist-get claim :file)))
      (should (file-exists-p state))
      (should (file-exists-p done))
      (should-not (string-match-p "Dirty report\." (buffer-string)))
      (should (= (envoy-org-collect-reports) 1))
      (should-not (file-exists-p (plist-get claim :file)))
      (should-not (file-exists-p state))
      (should-not (file-exists-p done))
      (should (string-match-p "Dirty report\." (buffer-string)))
      (should (= 1 (how-many "Dirty report\\." (point-min) (point-max)))))))
(ert-deftest envoy-test-org-collection-before-save-throw-rolls-back-and-retries-once ()
  "A before-save throw rolls back the report transition for retry."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO Before save throw\nSCHEDULED: <2026-09-19 Sat +1d>\n:PROPERTIES:\n:ID: before-save-throw\n:END:\n"
    (let* ((id (org-entry-get nil "ID"))
           (key (envoy-org--spool-key id))
           (claim (envoy-org--spool-claim id "Before save throw"))
           (state (envoy-org--spool-file
                   key envoy-org--spool-state-suffix))
           (done (envoy-org--spool-file
                  key envoy-org--spool-done-suffix))
           (before (buffer-string))
           (schedule-before (org-entry-get nil "SCHEDULED")))
      (envoy-org--spool-open "Before save throw" nil)
      (envoy-test--publish-spool-marker done "Before-save report.\n")
      (let ((before-save-hook
             (list (lambda ()
                     (throw 'envoy-test-before-save-throw 'caught)))))
        (should
         (eq (catch 'envoy-test-before-save-throw
               (envoy-org-collect-reports)
               :completed)
             'caught)))
      (should-not (buffer-modified-p))
      (should (equal before (buffer-string)))
      (should (equal schedule-before (org-entry-get nil "SCHEDULED")))
      (should (file-exists-p (plist-get claim :file)))
      (should (file-exists-p state))
      (should (file-exists-p done))
      (should-not (string-match-p "Before-save report\."
                                  (buffer-string)))
      (should (string-match-p "^\\* TODO Before save throw"
                              (envoy-test--file-text file)))
      (should (= (envoy-org-collect-reports) 1))
      (should-not (file-exists-p (plist-get claim :file)))
      (should-not (file-exists-p state))
      (should-not (file-exists-p done))
      (should (equal (org-get-todo-state) "TODO"))
      (should (equal "<2026-09-20 Sun +1d>"
                     (org-entry-get nil "SCHEDULED")))
      (should (= 1 (how-many "Before-save report\."
                             (point-min) (point-max)))))))

(ert-deftest envoy-test-org-collection-after-save-hook-owns-buffer-boundary ()
  "A post-save hook cannot capture another buffer's unsaved edits."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO Hook owner\nSCHEDULED: <2026-09-19 Sat +1d>\n:PROPERTIES:\n:ID: hook-owner\n:END:\n"
    (let* ((id (org-entry-get nil "ID"))
           (key (envoy-org--spool-key id))
           (claim (envoy-org--spool-claim id "Hook owner"))
           (state (envoy-org--spool-file
                   key envoy-org--spool-state-suffix))
           (done (envoy-org--spool-file
                  key envoy-org--spool-done-suffix))
           (other-file (expand-file-name "other.org" directory))
           other-buffer)
      (unwind-protect
          (progn
            (with-temp-file other-file (insert "Other base.\n"))
            (setq other-buffer (find-file-noselect other-file))
            (envoy-org--spool-open "Hook owner" nil)
            (envoy-test--publish-spool-marker done "Hook-owner report.\n")
            (let* ((fail-hook t)
                  (before-save-hook nil)
                  (after-save-hook
                   (list
                    (lambda ()
                      (when fail-hook
                        (setq fail-hook nil)
                        (with-current-buffer other-buffer
                          (goto-char (point-max))
                          (insert "Other buffer saved text.\n")
                          (save-buffer)
                          (insert "Other buffer unsaved text.\n"))
                        (with-current-buffer buffer
                          (goto-char (point-max))
                          (insert "Org hook mutation must roll back.\n"))
                        (error "After-save hook failed"))))))
              (should-error (envoy-org-collect-reports))
              (goto-char (point-min))
              (re-search-forward "^\\* TODO Hook owner")
              (org-back-to-heading t)
              (should-not (buffer-modified-p))
              (should (equal "<2026-09-20 Sun +1d>"
                             (org-entry-get nil "SCHEDULED")))
              (should-not (string-match-p "Org hook mutation must roll back\."
                                          (buffer-string)))
              (should (= 1 (how-many "Hook-owner report\."
                                     (point-min) (point-max))))
              (should (file-exists-p (plist-get claim :file)))
              (should (file-exists-p state))
              (should (file-exists-p done))
              (with-current-buffer other-buffer
                (should (buffer-modified-p))
                (should (string-match-p "Other buffer unsaved text\."
                                        (buffer-string))))
              (should-not (string-match-p "Other buffer unsaved text\."
                                          (envoy-test--file-text other-file)))
              (should (= (envoy-org-collect-reports) 1))
              (should-not (file-exists-p (plist-get claim :file)))
              (should-not (file-exists-p state))
              (should-not (file-exists-p done))
              (should (equal "<2026-09-20 Sun +1d>"
                             (org-entry-get nil "SCHEDULED")))
              (should (= 1 (how-many "Hook-owner report\."
                                     (point-min) (point-max))))
              (with-current-buffer other-buffer
                (should (buffer-modified-p))
                (should (string-match-p "Other buffer unsaved text\."
                                        (buffer-string))))))
        (when (buffer-live-p other-buffer)
          (with-current-buffer other-buffer
            (set-buffer-modified-p nil))
          (kill-buffer other-buffer))
        (when (file-exists-p other-file)
          (delete-file other-file))))))

(ert-deftest envoy-test-org-collection-after-save-hook-error-keeps-evidence ()
  "A post-save hook error retains the durable transition for a safe retry."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO Hook failure\nSCHEDULED: <2026-09-19 Sat +1d>\n:PROPERTIES:\n:ID: hook-failure\n:END:\n"
    (let* ((id (org-entry-get nil "ID"))
           (key (envoy-org--spool-key id))
           (claim (envoy-org--spool-claim id "Hook failure"))
           (state (envoy-org--spool-file
                   key envoy-org--spool-state-suffix))
           (done (envoy-org--spool-file
                  key envoy-org--spool-done-suffix))
           (schedule-before (org-entry-get nil "SCHEDULED")))
      (envoy-org--spool-open "Hook failure" nil)
      (envoy-test--publish-spool-marker done "Hook report.\n")
      (let* ((fail-hook t)
            (after-save-hook
             (list (lambda ()
                     (when fail-hook
                       (setq fail-hook nil)
                       (error "After-save hook failed"))))))
        (should-error (envoy-org-collect-reports))
        (should-not (buffer-modified-p))
        (should (file-exists-p (plist-get claim :file)))
        (should (file-exists-p state))
        (should (file-exists-p done))
        (should (equal (org-get-todo-state) "TODO"))
        (should (equal "<2026-09-19 Sat +1d>" schedule-before))
        (should (equal "<2026-09-20 Sun +1d>"
                       (org-entry-get nil "SCHEDULED")))
        (let ((schedule-after-save (org-entry-get nil "SCHEDULED")))
          (let ((saved-text (envoy-test--file-text file)))
            (should (string-match-p "^\\* TODO Hook failure" saved-text))
            (should (string-match-p (regexp-quote schedule-after-save)
                                    saved-text)))
          (goto-char (point-max))
          (insert "Intervening user text survives retry.\n")
          (should (buffer-modified-p))
          (should (= (envoy-org-collect-reports) 1))
          (should-not (file-exists-p (plist-get claim :file)))
          (should-not (file-exists-p state))
          (should-not (file-exists-p done))
          (should (equal schedule-after-save (org-entry-get nil "SCHEDULED")))
          (should (equal (org-get-todo-state) "TODO"))
          (should (string-match-p "Intervening user text survives retry\."
                                  (buffer-string)))
          (should (string-match-p "Intervening user text survives retry\."
                                  (envoy-test--file-text file)))
          (should (= 1 (how-many "Hook report\." (point-min) (point-max))))
          (should (= (envoy-org-collect-reports) 0)))))))

(ert-deftest envoy-test-org-collection-cleanup-failure-retries-same-and-reopened-buffer ()
  "Cleanup failure is safe to retry in the current and a reopened buffer."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO Same buffer\nSCHEDULED: <2026-09-19 Sat +1d>\n:PROPERTIES:\n:ID: cleanup-same\n:END:\n* TODO Reopened\nSCHEDULED: <2026-09-19 Sat +1d>\n:PROPERTIES:\n:ID: cleanup-reopened\n:END:\n"
    (let ((real-delete-file (symbol-function 'delete-file)))
      (goto-char (point-min))
      (re-search-forward "^\\* TODO Same buffer")
      (org-back-to-heading t)
      (let* ((id (org-entry-get nil "ID"))
             (key (envoy-org--spool-key id))
             (claim (envoy-org--spool-claim id "Same buffer"))
             (state (envoy-org--spool-file
                     key envoy-org--spool-state-suffix))
             (done (envoy-org--spool-file
                    key envoy-org--spool-done-suffix)))
        (envoy-org--spool-open "Same buffer" nil)
        (envoy-test--publish-spool-marker done "Same-buffer report.\n")
        (let ((blocked nil))
          (cl-letf (((symbol-function 'delete-file)
                     (lambda (path)
                       (if (and (not blocked)
                                (equal path state))
                           (progn
                             (setq blocked t)
                             (error "Cleanup failed"))
                         (funcall real-delete-file path)))))
            (should-error (envoy-org-collect-reports)))
          (should blocked))
        (should-not (buffer-modified-p))
        (should (file-exists-p (plist-get claim :file)))
        (should (file-exists-p state))
        (should (file-exists-p done))
        (let ((schedule-after-save (org-entry-get nil "SCHEDULED")))
          (should (equal "<2026-09-20 Sun +1d>" schedule-after-save))
          (should (= (envoy-org-collect-reports) 1))
          (should (equal schedule-after-save (org-entry-get nil "SCHEDULED")))
          (should-not (file-exists-p (plist-get claim :file)))
          (should-not (file-exists-p state))
          (should-not (file-exists-p done))
          (should (= 1 (how-many "Same-buffer report\\."
                                 (point-min) (point-max))))))
      (goto-char (point-min))
      (re-search-forward "^\\* TODO Reopened")
      (org-back-to-heading t)
      (let* ((id (org-entry-get nil "ID"))
             (key (envoy-org--spool-key id))
             (claim (envoy-org--spool-claim id "Reopened"))
             (state (envoy-org--spool-file
                     key envoy-org--spool-state-suffix))
             (done (envoy-org--spool-file
                    key envoy-org--spool-done-suffix)))
        (envoy-org--spool-open "Reopened" nil)
        (envoy-test--publish-spool-marker done "Reopened report.\n")
        (let ((blocked nil))
          (cl-letf (((symbol-function 'delete-file)
                     (lambda (path)
                       (if (and (not blocked)
                                (equal path state))
                           (progn
                             (setq blocked t)
                             (error "Cleanup failed"))
                         (funcall real-delete-file path)))))
            (should-error (envoy-org-collect-reports)))
          (should blocked))
        (should (file-exists-p (plist-get claim :file)))
        (should (file-exists-p state))
        (should (file-exists-p done))
        (let ((old-buffer buffer))
          (kill-buffer old-buffer)
          (setq buffer (find-file-noselect file)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "^\\* TODO Reopened")
          (org-back-to-heading t)
          (let ((schedule-after-save (org-entry-get nil "SCHEDULED")))
            (should (equal "<2026-09-20 Sun +1d>" schedule-after-save))
            (should (= (envoy-org-collect-reports) 1))
            (should (equal schedule-after-save
                           (org-entry-get nil "SCHEDULED")))
            (should-not (file-exists-p (plist-get claim :file)))
            (should-not (file-exists-p state))
            (should-not (file-exists-p done))
            (should (= 1 (how-many "Reopened report\\."
                                   (point-min) (point-max))))))))))

(ert-deftest envoy-test-org-fresh-identical-marker-advances-repeat ()
  "A new marker with the same prose advances the next recurrence."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO Repeat\nSCHEDULED: <2026-09-19 Sat +1d>\n:PROPERTIES:\n:ID: identical-marker\n:END:\n"
    (let* ((id (org-entry-get nil "ID"))
           (key (envoy-org--spool-key id))
           (claim-one (envoy-org--spool-claim id "Repeat"))
           (done (envoy-org--spool-file
                  key envoy-org--spool-done-suffix)))
      (envoy-org--spool-open "Repeat" nil)
      (envoy-test--publish-spool-marker done "Identical report.\n")
      (should (= (envoy-org-collect-reports) 1))
      (let ((schedule-one (org-entry-get nil "SCHEDULED")))
        (should (equal "<2026-09-20 Sun +1d>" schedule-one))
        (let* ((claim-two (envoy-org--spool-claim id "Repeat"))
               (_state-two (envoy-org--spool-open "Repeat" nil)))
          (should claim-one)
          (should claim-two)
          (envoy-test--publish-spool-marker done "Identical report.\n")
          (should (= (envoy-org-collect-reports) 1))
          (should (equal "<2026-09-21 Mon +1d>"
                         (org-entry-get nil "SCHEDULED")))
          (should-not (file-exists-p (plist-get claim-two :file)))
          (should-not (file-exists-p done))
          (should (= 1 (how-many "Identical report\\."
                                 (point-min) (point-max)))))))))

(ert-deftest envoy-test-tmux-post-launch-typed-failure-keeps-claim-and-state ()
  "A typed brief failure after launch keeps terminal evidence for collection."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file
        "#+TODO: TODO DOING | DONE\n* TODO Typed failure\n:PROPERTIES:\n:ID: typed-failure\n:END:\n"
      (let ((envoy-tmux-providers
             '((claude :program "true" :name "Claude" :args ()
                       :prompt-arg nil :variable setup :setup nil :setups ()))))
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "test:7"))
                  ((symbol-function 'envoy-tmux-type)
                   (lambda (&rest _)
                     (error "Typed brief failed")))
                  ((symbol-function 'envoy-org--attach-directory)
                   (lambda () nil))
                  ((symbol-function 'message)
                   (lambda (&rest _) nil)))
          (should-error
           (envoy-tmux-org-heading-to 'claude nil nil nil "test")))
        (let* ((key (envoy-org--spool-key (org-entry-get nil "ID")))
               (claim (envoy-org--spool-file
                       key envoy-org--spool-claim-suffix))
               (state (envoy-org--spool-file
                       key envoy-org--spool-state-suffix)))
          (should (file-exists-p claim))
          (should (file-exists-p state))
          (should (equal (org-get-todo-state) "TODO")))))))

(ert-deftest envoy-test-tmux-post-launch-readiness-failure-keeps-claim-and-state ()
  "A readiness failure after launch keeps terminal evidence for collection."
  (envoy-test--with-tmux '()
    (envoy-test--with-org-file
        "#+TODO: TODO DOING | DONE\n* TODO Readiness failure\n:PROPERTIES:\n:ID: readiness-failure\n:END:\n"
      (let ((envoy-tmux-providers
             '((codex :program "true" :name "Codex" :args ()
                      :prompt-arg nil :variable setup :setup nil :setups ()
                      :opening-input ("")))))
        (cl-letf (((symbol-function 'envoy-tmux-window)
                   (lambda (&rest _) "test:7"))
                  ((symbol-function 'envoy-tmux--wait-for-agent)
                   (lambda (&rest _) t))
                  ((symbol-function 'envoy-tmux--wait-for-interface)
                   (lambda (&rest _)
                     (user-error "Readiness failed")))
                  ((symbol-function 'envoy-org--attach-directory)
                   (lambda () nil))
                  ((symbol-function 'message)
                   (lambda (&rest _) nil)))
          (should-error
           (envoy-tmux-org-heading-to 'codex nil nil nil "test")
           :type 'user-error))
        (let* ((key (envoy-org--spool-key (org-entry-get nil "ID")))
               (claim (envoy-org--spool-file
                       key envoy-org--spool-claim-suffix))
               (state (envoy-org--spool-file
                       key envoy-org--spool-state-suffix)))
          (should (file-exists-p claim))
          (should (file-exists-p state))
          (should (equal (org-get-todo-state) "TODO")))))))

(ert-deftest envoy-test-tmux-post-launch-active-keyword-failure-keeps-claim ()
  "An active-keyword failure after launch keeps the terminal claim."
  (envoy-test--with-org-file
      "#+TODO: TODO DOING | DONE\n* TODO Active keyword failure\n:PROPERTIES:\n:ID: active-keyword-failure\n:END:\n"
    (let ((envoy-tmux-providers
           '((claude :program "true" :name "Claude" :args ()
                     :prompt-arg nil :variable setup :setup nil :setups ()))))
      (cl-letf (((symbol-function 'envoy-tmux-window)
                 (lambda (&rest _) "test:7"))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (_target _prompt &optional _provider _model _setup
                                   _plan-into on-start _claim &rest _)
                   (when on-start (funcall on-start))))
                ((symbol-function 'envoy-org--set-keyword)
                 (lambda (&rest _)
                   (error "Active keyword failed")))
                ((symbol-function 'envoy-org--attach-directory)
                 (lambda () nil))
                ((symbol-function 'message)
                 (lambda (&rest _) nil)))
        (should-error
         (envoy-tmux-org-heading-to 'claude nil nil nil "test")))
      (let* ((key (envoy-org--spool-key (org-entry-get nil "ID")))
             (claim (envoy-org--spool-file
                     key envoy-org--spool-claim-suffix))
             (state (envoy-org--spool-file
                     key envoy-org--spool-state-suffix)))
        (should (file-exists-p claim))
        (should (file-exists-p state))
        (should (equal (org-get-todo-state) "TODO"))))))

;;; Dired dispatch

(require 'envoy-dired)

(defmacro envoy-test--with-dired-files (names &rest body)
  "Run BODY in Dired for temporary files named NAMES."
  (declare (indent 1))
  `(let* ((directory (make-temp-file "envoy-test-dired-" t))
          (dired-buffer nil))
     (unwind-protect
         (progn
           (dolist (name ,names)
             (with-temp-file (expand-file-name name directory)
               (insert "* Example\n")))
           (setq dired-buffer (dired-noselect directory))
           (with-current-buffer dired-buffer
             ,@body))
       (when (buffer-live-p dired-buffer)
         (kill-buffer dired-buffer))
       (delete-directory directory t))))

(ert-deftest envoy-test-dired-prefix-dispatches-in-prompt-order ()
  "A Dired dispatch asks for direction, selection, and session before launch."
  (envoy-test--with-dired-files '("task.org")
    (let* ((path (expand-file-name "task.org" default-directory))
           (envoy-tmux-providers
            '((claude :program "true" :name "Claude" :variable setup)))
           (events nil)
           sent window-title)
      (dired-goto-file path)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (prompt &rest _)
                   (setq events (append events '(direction)))
                   (should (equal prompt "Direction for Claude: "))
                   "Inspect the Org file"))
                ((symbol-function 'envoy-tmux--read-variable)
                 (lambda (provider)
                   (setq events (append events '(selection)))
                   (should (eq provider 'claude))
                   '(:model nil :setup "careful" :plan-into nil)))
                ((symbol-function 'envoy-tmux-read-session)
                 (lambda (provider)
                   (setq events (append events '(session)))
                   (should (eq provider 'claude))
                   "work"))
                ((symbol-function 'envoy-tmux-window)
                 (lambda (session title)
                   (setq events (append events '(window))
                         window-title title)
                   (should (equal session "work"))
                   "work:3"))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (target prompt provider model setup plan-into &rest _)
                   (setq events (append events '(send))
                         sent (list target prompt provider model setup
                                    plan-into))))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (should (equal (envoy-dired-claude '(4)) "work:3"))
        (should (equal events '(direction selection session window send)))
        (should (equal window-title "task.org"))
        (should (equal (nth 0 sent) "work:3"))
        (should (eq (nth 2 sent) 'claude))
        (should (equal (nth 4 sent) "careful"))
        (should (string-prefix-p
                 "USER DIRECTION\nInspect the Org file\n\n" (nth 1 sent)))))))

(ert-deftest envoy-test-dired-marked-files-are-all-named-in-brief ()
  "Every marked Dired file appears in the brief as an absolute JSON string."
  (envoy-test--with-dired-files '("a-task.org" "b-notes.txt")
    (let* ((envoy-tmux-providers
            '((claude :program "true" :name "Claude")))
           (first (expand-file-name "a-task.org" default-directory))
           (second (expand-file-name "b-notes.txt" default-directory))
           (files nil)
           (prompt nil)
           (title nil))
      (dired-goto-file first)
      (dired-mark 1)
      (dired-goto-file second)
      (dired-mark 1)
      (setq files (dired-get-marked-files))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "Review these files"))
                ((symbol-function 'envoy-tmux-read-session)
                 (lambda (&rest _) "work"))
                ((symbol-function 'envoy-tmux-window)
                 (lambda (_session window-name)
                   (setq title window-name)
                   "work:4"))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (_target brief &rest _)
                   (setq prompt brief)))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (envoy-dired-claude nil))
      (dolist (path files)
        (should (string-match-p
                 (regexp-quote (json-encode-string (expand-file-name path)))
                 prompt)))
      (should (equal title
                     (format "%s (+%d more)"
                             (file-name-nondirectory
                              (directory-file-name (car files)))
                             (1- (length files))))))))

(ert-deftest envoy-test-dired-empty-direction-uses-default ()
  "An empty direction uses `envoy-default-direction' in the brief."
  (envoy-test--with-dired-files '("task.org")
    (let ((envoy-default-direction "Use the fallback direction")
          (envoy-tmux-providers '((claude :program "true" :name "Claude")))
          (prompt nil))
      (dired-goto-file (expand-file-name "task.org" default-directory))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'envoy-tmux-read-session)
                 (lambda (&rest _) "work"))
                ((symbol-function 'envoy-tmux-window)
                 (lambda (&rest _) "work:5"))
                ((symbol-function 'envoy-tmux-send)
                 (lambda (_target brief &rest _) (setq prompt brief)))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (envoy-dired-claude nil))
      (should (string-prefix-p
               "USER DIRECTION\nUse the fallback direction\n\n" prompt)))))

(ert-deftest envoy-test-dired-rejects-empty-file-list-before-prompt ()
  "An empty Dired buffer is rejected before asking for direction."
  (envoy-test--with-dired-files nil
    (let ((prompt-called nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (setq prompt-called t) "Continue")))
        (should-error (envoy-dired-to 'claude) :type 'user-error))
      (should-not prompt-called))))

(ert-deftest envoy-test-dired-rejects-non-dired-buffer ()
  "Dired dispatch is available only from a Dired buffer."
  (with-temp-buffer
    (should-error (envoy-dired-to 'claude) :type 'user-error)))

(ert-deftest envoy-test-dired-missing-provider-errors-before-prompt ()
  "A missing provider executable fails before asking for direction."
  (envoy-test--with-dired-files '("task.org")
    (let ((envoy-tmux-providers
           '((claude :program "/not/a/real/envoy-program" :name "Claude")))
          (prompt-called nil))
      (dired-goto-file (expand-file-name "task.org" default-directory))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (setq prompt-called t) "Continue")))
        (should-error (envoy-dired-to 'claude) :type 'user-error))
      (should-not prompt-called))))

(ert-deftest envoy-test-dired-setup-keys-binds-all-dispatches ()
  "The setup command binds the six Dired shortcuts."
  (let ((dired-mode-map (copy-keymap dired-mode-map)))
    (envoy-dired-setup-keys)
    (dolist (binding '(("C-c C-x A" . envoy-dired-claude)
                       ("C-c C-x R" . envoy-dired-reasonix)
                       ("C-c C-x P" . envoy-dired-pi)
                       ("C-c C-x C" . envoy-dired-codex)
                       ("C-c C-x O" . envoy-dired-omp)
                       ("C-c C-x S" . envoy-dired-dsh)))
      (should (eq (lookup-key dired-mode-map (kbd (car binding)))
                  (cdr binding))))))

(ert-deftest envoy-test-dired-omp-checks-program-before-prompt ()
  "An unavailable OMP program fails before asking for direction."
  (envoy-test--with-dired-files '("task.org")
    (let ((envoy-tmux-providers '((omp :program "true" :name "OMP")))
          (prompt-called nil))
      (dired-goto-file (expand-file-name "task.org" default-directory))
      (cl-letf (((symbol-function 'envoy-tmux--omp-program)
                 (lambda () (user-error "OMP unavailable")))
                ((symbol-function 'read-string)
                 (lambda (&rest _) (setq prompt-called t) "Continue")))
        (should-error (envoy-dired-to 'omp) :type 'user-error))
      (should-not prompt-called))))

(defmacro envoy-test--with-agenda-file (text heading &rest body)
  "Evaluate BODY on HEADING in an Org TODO agenda generated from TEXT."
  (declare (indent 2))
  `(envoy-test--with-org-file ,text
     (let ((org-agenda-files (list file))
           agenda-buffer)
       (unwind-protect
           (save-window-excursion
             (org-todo-list)
             (setq agenda-buffer (current-buffer))
             (with-current-buffer agenda-buffer
               (goto-char (point-min))
               (search-forward ,heading)
               (beginning-of-line)
               ,@body))
         (when (buffer-live-p agenda-buffer)
           (kill-buffer agenda-buffer))))))

(ert-deftest envoy-test-agenda-omp-prefix-dispatches-from-source-heading ()
  "OMP prefix and agenda guards dispatch from the selected source heading."
  (envoy-test--with-tmux '("agenda-test")
    (envoy-test--with-agenda-file
        "#+TODO: TODO DOING | DONE\n* Parent context\nParent details.\n** TODO Chosen subtree\nChosen details.\n*** TODO Chosen child\nChild details.\n** TODO Sibling secret\nSibling details.\n"
        "Chosen subtree"
      (let ((source-buffer buffer)
            (agenda-buffer (current-buffer))
            (agenda-point (point))
            (agenda-min (point-min))
            (agenda-max (point-max))
            source-point source-min source-max prompt-file
            (envoy-tmux-provider 'omp)
            (envoy-tmux-providers
             '((omp :program "true" :name "OMP" :args ()
                    :prompt-arg "@%s" :model-arg "--model"
                    :effort-arg "--thinking" :efforts ("high")
                    :variable model :setup nil :models ("selected/model"))))
            (org-attach-auto-tag "ATTACH"))
        (with-current-buffer source-buffer
          (goto-char (point-min))
          (search-forward "** TODO Chosen subtree")
          (beginning-of-line)
          (narrow-to-region (point) (point-max))
          (setq source-point (copy-marker (point))
                source-min (copy-marker (point-min))
                source-max (copy-marker (point-max) t)))
        (unwind-protect
            (progn
              (goto-char (point-min))
              (while (and (< (point) (point-max))
                          (or (get-text-property (point) 'org-hd-marker)
                              (get-text-property (line-beginning-position)
                                                 'org-marker)))
                (forward-line 1))
              (should (< (point) (point-max)))
              (should-not (or (get-text-property (point) 'org-hd-marker)
                              (get-text-property (line-beginning-position)
                                                 'org-marker)))
              (let ((prompt-called nil))
                (cl-letf (((symbol-function 'envoy--read-direction)
                           (lambda (&rest _)
                             (setq prompt-called t)
                             (user-error "Unexpected direction prompt"))))
                  (should-error
                   (call-interactively #'envoy-tmux-org-heading-omp)
                   :type 'user-error))
                (should-not prompt-called)
                (should-not (envoy-test--tmux-calls)))
              (goto-char agenda-point)
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (prompt _choices &rest _)
                           (cond
                            ((string-prefix-p "OMP launch mode" prompt)
                             "Single model")
                            ((string-prefix-p "Model for OMP" prompt)
                             "selected/model")
                            ((string-prefix-p "Thinking effort" prompt) "high")
                            ((string-prefix-p "tmux session" prompt)
                             "agenda-test")
                            (t (error "Unexpected completion prompt: %s"
                                      prompt))))))
                (let ((current-prefix-arg '(4)))
                  (call-interactively #'envoy-tmux-org-heading-omp)))
              (should (eq (current-buffer) agenda-buffer))
              (should (= (point) agenda-point))
              (should (= (point-min) agenda-min))
              (should (= (point-max) agenda-max))
              (let* ((send (seq-find
                            (lambda (call)
                              (and (equal (car call) "send-keys")
                                   (member "-l" call)))
                            (envoy-test--tmux-calls)))
                     (command (nth 4 send)))
                (should send)
                (should
                 (string-match-p
                  (regexp-quote
                   (concat "--thinking high "
                           (shell-quote-argument
                            "## From an Emacs org heading: Chosen subtree")))
                  command))
                (let ((matched
                       (string-match "@\\([^ ;]+\\)"
                                     command)))
                  (setq prompt-file (and matched (match-string 1 command)))
                  (should prompt-file)))
              (let ((brief (with-temp-buffer
                             (insert-file-contents prompt-file)
                             (buffer-string))))
                (should (string-match-p "Parent context" brief))
                (should (string-match-p "Chosen child" brief))
                (should (string-match-p "Child details" brief))
                (should-not (string-match-p "Sibling secret" brief)))
              (with-current-buffer source-buffer
                (should (= (point) (marker-position source-point)))
                (should (= (point-min) (marker-position source-min)))
                (should (= (point-max) (marker-position source-max)))
                (save-excursion
                  (goto-char source-point)
                  (should (equal (org-get-todo-state) "DOING"))
                  (should (member "ATTACH" (org-get-tags nil t)))
                  (should (file-directory-p (org-attach-dir nil t)))
                  (goto-char (point-min))
                  (re-search-forward "^\\*\\* TODO Sibling secret$")
                  (org-back-to-heading t)
                  (should (equal (org-get-todo-state) "TODO"))
                  (should-not (member "ATTACH" (org-get-tags nil t))))))
          (when (and prompt-file (file-exists-p prompt-file))
            (delete-file prompt-file)))))))

(ert-deftest envoy-test-agenda-subprocess-files-report-under-selected-source ()
  "A completed subprocess report stays under its selected agenda heading."
  (envoy-test--with-agenda-file
      "#+TODO: TODO DOING | DONE\n* Parent context\nParent details.\n** TODO Chosen subtree\nChosen details.\n*** TODO Chosen child\nChild details.\n** TODO Sibling secret\nSibling details.\n"
      "Chosen subtree"
    (let* ((source-buffer buffer)
           (agenda-buffer (current-buffer))
           (agenda-point (point))
           (agenda-min (point-min))
           (agenda-max (point-max))
           (brief-file (expand-file-name "agent-input.txt" directory))
           (agent-script
            (envoy-test--agent
             "{\"type\":\"result\",\"is_error\":false,\"result\":\"ENVOY_STATUS: complete\\nSelected report.\"}"
             nil nil brief-file))
           source-point source-min source-max)
      (with-current-buffer source-buffer
        (goto-char (point-min))
        (search-forward "** TODO Chosen subtree")
        (beginning-of-line)
        (narrow-to-region (point) (point-max))
        (setq source-point (copy-marker (point))
              source-min (copy-marker (point-min))
              source-max (copy-marker (point-max) t)))
      (envoy-test--with-agent agent-script
        (let ((before (process-list)) process)
          (unwind-protect
              (progn
                (envoy-org-heading)
                (setq process
                      (seq-find
                       (lambda (candidate)
                         (and (not (memq candidate before))
                              (equal (car-safe (process-command candidate))
                                     agent-script)))
                       (process-list)))
                (should process)
                (with-timeout (10 (error "Envoy-test: agent did not finish"))
                  (while (not
                          (with-current-buffer source-buffer
                            (save-excursion
                              (save-restriction
                                (widen)
                                (goto-char source-point)
                                (equal (org-get-todo-state) "DONE")))))
                    (accept-process-output process 0.05)))
                (should (eq (current-buffer) agenda-buffer))
                (should (= (point) agenda-point))
                (should (= (point-min) agenda-min))
                (should (= (point-max) agenda-max))
                (let ((brief (with-temp-buffer
                               (insert-file-contents brief-file)
                               (buffer-string))))
                  (should (string-match-p "Parent context" brief))
                  (should (string-match-p "Chosen child" brief))
                  (should (string-match-p "Child details" brief))
                  (should-not (string-match-p "Sibling secret" brief)))
                (with-current-buffer source-buffer
                  (should (= (point) (marker-position source-point)))
                  (should (= (point-min) (marker-position source-min)))
                  (should (= (point-max) (marker-position source-max)))
                  (save-restriction
                    (widen)
                    (save-excursion
                      (goto-char source-point)
                      (should (equal (org-get-todo-state) "DONE"))
                      (let ((end (save-excursion
                                   (org-end-of-subtree t t)
                                   (point))))
                        (should (search-forward ":ENVOY:" end t))
                        (goto-char source-point)
                        (should (search-forward "Selected report." end t)))
                      (re-search-forward "^\\*\\* TODO Sibling secret$")
                      (org-back-to-heading t)
                      (should (equal (org-get-todo-state) "TODO"))
                      (let ((end (save-excursion
                                   (org-end-of-subtree t t)
                                   (point))))
                        (should-not (search-forward "Selected report." end t)))))))
            (dolist (candidate (process-list))
              (when (and (not (memq candidate before))
                         (equal (car-safe (process-command candidate))
                                agent-script)
                         (process-live-p candidate))
                (delete-process candidate)))))))))
