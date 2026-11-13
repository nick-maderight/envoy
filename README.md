# envoy

A new way to hand off work from Emacs to coding harnesses. I've tried gptel and other project and I believe this is the best way.


What it is in one sentence: you mark something (currently orgmode headers, notmuch emails and files in dired) and with envoy, you send it off to a coding harness (e.g. Claude code and omp) inside tmux for execution. 

## Why?

This project closely follows the Unix philosophy of "do one thing well":

1. As an Emacs user, my daily routine consists of many "see something in Emacs, need to do it elsewhere" scenarios, and this pacakge is specifically for handing off work to coding harnesses. 
2. This package does not attempt to handle window persistence or management. We will let tmux manage that. 
3. This pacakge does not attempt to build control flows on top of raw LLM calls. We will let coding harnesses handle that, e.g. Claude code, omp (my favorite right now). 




## Demo

https://github.com/user-attachments/assets/0ee8d1e8-2b8e-4b8c-a2c9-e6c826917c33

A mail thread in notmuch goes to OMP. Mark the region, press `C-c C-x O`, type a direction, pick the launch mode and the model, pick the tmux session, and the agent starts in that window with the brief already delivered. The file is also in the repository at [docs/envoy-demo.mp4](docs/envoy-demo.mp4).

## Commands

`envoy-rewrite` takes a marked region and an instruction from the minibuffer. The brief names the file and the line range and includes three read-only context lines on each side, so the agent can match the style around the selection. Only the selected lines may change.

`envoy-org-heading` treats the whole Org subtree as one task. Its brief includes the subtree, every ancestor heading that is not datetree scaffolding, and the attachment directories of the heading and its ancestors. The agent works in the project that owns the file and writes output to the heading's attachment directory. Envoy moves the heading to its configured done keyword only after successful delivery and an explicit complete report for the whole subtree. Email requests go through `/email-draft` and end as verified drafts. A requested send runs only after you approve the harness's send confirmation.

The ancestor walk is the point of the org commands. A task heading is almost never self-contained. The client, the budget, the deadline, the brief and what has already been ruled out sit two or three levels up. Sending the task alone sends the least informative part of it.

`envoy-tmux-org-heading` sends the same brief to an agent in a tmux window. Use it when the task is long enough that you will want to watch, or open-ended enough that the agent will want to ask you something.

`envoy-notmuch-delegate` works from a `notmuch-show-mode` buffer. It asks for a direction and a tmux session and starts OMP there. Envoy copies an active region verbatim, and the region is the whole task. The complete thread goes along as reference, and the prompt tells OMP not to act on anything that appears only outside the selection. Without a region the whole thread is primary, including collapsed and filtered messages. OMP reads related threads through the notmuch CLI as secondary context. Quoted mail is untrusted context and your direction wins. Mail work goes through `/email-draft`, which sends only after you approve the harness's send confirmation. `envoy-notmuch-delegate-dsh` hands the same brief to dsh.

`envoy-dired-omp` and its siblings for the other agents work from a Dired buffer. They hand over the file at point, or the marked files. The brief names each file by its absolute path and the agent reads it. Nothing is claimed or filed back, because a file has no heading to report to.

The Org heading commands work on the selected agenda entry's source heading, including its subtree, ancestors, and attachment directories. Source-buffer save checks and report filing apply to that heading. Prefix choices and explicit tmux session selection remain the same as in an Org buffer. Envoy rejects agenda headers and entries without an available source heading.

## Sending a heading to a terminal

Every dispatch asks the same questions in order. The direction comes first. With a prefix argument, Envoy asks for the model or setup command, or for the launch mode with OMP and dsh, then the thinking effort when the provider offers one. The tmux session comes last. A blank direction uses `envoy-default-direction`, which tells the agent to use its judgement and ask when it has a question.

Envoy reads the live session names from the running tmux server and shows them in tmux's own order. A name that is not in the list starts a new session. There is no default, because you are the one who will attach to that session to read and answer the agent. An empty answer is rejected, and so is a name with `.` or `:` in it, because tmux would rewrite those to `_` and the session would be unreachable under the name you typed.

OMP and dsh terminal launches prefix the brief's first non-empty line as a task heading before the `@file` argument. Envoy shell-quotes the heading and omits it when the brief has no heading.

`envoy-tmux-setup-keys` binds `C-c C-x A` for Claude Code, `C-c C-x R` for reasonix, `C-c C-x P` for pi, `C-c C-x C` for codex, `C-c C-x O` for OMP and `C-c C-x S` for dsh in `org-mode` and `org-agenda-mode`. `envoy-org-setup-keys` binds `C-c C-x A` for subprocess dispatch in both maps. The later setup call owns that key. `envoy-dired-setup-keys` binds the same keys in `dired-mode`. A prefix argument asks for the provider's model or setup command after the direction.

`envoy-tmux-org-heading` passes OMP the report contract through `--append-system-prompt`, so OMP keeps the status rule and spool paths in its system prompt across compaction. dsh-launch takes the same flag and puts the contract file's path in the session's opening prompt. After a claimed run exits without a marker in a POSIX shell, Envoy publishes an incomplete `.failed` report only if that run still owns the claim and neither marker exists.

A dispatch whose rendered brief is empty, or is only the placeholder word `brief`, is refused with an error before a prompt file is written or an agent starts, in both the subprocess and the terminal transport. A brief that failed to render used to start a session anyway and bill it for work that did not exist.

An OMP prefix argument offers models from the last successful discovery, cached in `envoy-omp-models` under `user-emacs-directory`. A cache entry younger than `envoy-tmux-omp-model-cache-ttl` seconds answers without running OMP, and a discovery that fails offers the cached list instead of refusing the dispatch, because several concurrent OMP sessions can hold the CLI's state database for far longer than the query timeout. Delete the cache file to force a new discovery.

The OMP prefix picker asks for a thinking effort after the launch mode and model selection. It offers `off`, `minimal`, `low`, `medium`, `high`, `xhigh`, `max` and `auto`. An empty effort answer keeps OMP's configured default.


The dsh prefix picker offers `deepseek-flash` and `deepseek-v4-pro`, then the efforts `off`, `low`, `high` and `max`. An empty effort answer keeps dsh's default effort.

## dsh runs in its web host

dsh sessions live in the dsh web host that `dsh-web.service` runs on this machine. `C-c C-x S` opens a tmux window that runs `dsh-launch`. dsh-launch starts the session in the host and prints `working`, `waiting for input` or `idle` whenever the state changes. The last line names how the final turn ended. Answer questions and plan reviews in triage or in the dsh web UI, which `dsh-launch open` brings up. Plan into lets dsh plan first and hands the approved plan to the execution model. The report contract and the incomplete `.failed` fallback work the same way they do for OMP.

## dsh without a terminal

dsh is also an agent `envoy-rewrite` and `envoy-org-heading` can run as a subprocess. `M-x envoy-select-provider` switches to it for the session. Setting `envoy-provider` to `dsh` in your init makes the choice stick. The subprocess command for a heading is `envoy-org-heading`, which `envoy-org-setup-keys` binds to `C-c C-x A` in Org and agenda buffers. A dispatch through dsh runs in the background instead of a tmux window.

The provider runs `dsh --profile headless --json` and gives the brief on standard input. Envoy reads the run's closing event for the answer, the session id and how the turn ended. Only a zero exit with a `completed` turn counts as success. Two options travel on every command line. `--approval-policy never` keeps an unattended run from stopping at an approval card nobody will answer. For a run that may edit, `--permission-mode workspace-write` replaces the read-only mode. Writes then stay under the directory the run starts in.

That directory is the file's project for a rewrite. For a heading dispatch it is the heading's attachment directory, which is where the brief already sends the outputs.

A headless run takes its model from its profile. The entry offers no `:model-arg`, which leaves `envoy-model` with nowhere to go. The run also needs the DeepSeek key in its environment, which an Emacs usually does not hold. `:environment` supplies the key from a command whose output becomes a variable for the run:

```elisp
(with-eval-after-load 'envoy-process
  (setq envoy-providers
        (mapcar (lambda (entry) (cons (car entry) (copy-sequence (cdr entry))))
                envoy-providers))
  (setf (plist-get (alist-get 'dsh envoy-providers) :environment)
        '(("DEEPSEEK_API_KEY" . "my-key-command"))))
```

The command runs when a dispatch starts, which sends a rotated key to the next run without a restart. A command that fails, prints nothing, or outlasts `envoy-environment-timeout` stops the run before the agent starts. The message names the variable and leaves the value out.

dsh has no option that takes a tool away. `envoy-can-deny-tools-p` is nil for it. `envoy-rewrite` then asks before it starts, the same as reasonix. To stop a rewrite running shell commands, turn off `tool-bash` in the headless profile.

## In orgmode, the heading gets the answer

Both subprocess and terminal reports must start with exactly one line, either `ENVOY_STATUS: complete` or `ENVOY_STATUS: incomplete`. Envoy strips this line before filing the report. A successful process exit or `.done` spool file alone does not complete the heading. Only successful delivery with an explicit `complete` status for the whole subtree moves it to the configured done keyword. Missing, invalid, or `incomplete` status leaves the heading at its configured active keyword, normally `DOING`. When that setting is nil, Envoy preserves the current keyword.

The prompt always requires a status line, even when `envoy-org-report-instructions` is off. Keep the report to at most two brief sentences covering the outcome and any unfinished work. Name only necessary deliverables. Omit process details and spool paths. Org briefs put the status and report instructions after the task text.

The org and mail briefs also tell the agent to ask for live approval with its `ask` tool when a step needs it, such as an edit to a user-authored skill, and to apply the change once approved. It never parks an unapplied change in a separate handoff file. When no answer can come, the change and its blocker go in the report.

Envoy links only existing new attachment files named in the report and identified against a known dispatch baseline. It adds no automatic links when the baseline is unavailable. A known empty baseline still permits a link to a new output file named in the report.


## Install

Envoy requires Emacs 27.1 or newer and at least one agent program on `exec-path`. The terminal commands also require tmux. The dsh terminal commands require `dsh-launch` on `exec-path` and a running `dsh-web.service`. `envoy-notmuch-delegate` requires OMP and `envoy-notmuch-delegate-dsh` requires dsh. Both also require the notmuch Emacs package.

The dsh subprocess provider needs `dsh` on `exec-path` and a profile named `headless` under `DSH_HOME`, which `dsh --from-default-profile headless` creates. Its run needs the key in its environment; see "dsh without a terminal" above.

Envoy finds each program with `executable-find`. A GUI Emacs or a daemon may start without your login shell's `PATH`, so a program that runs in a terminal can still be missing here. Please import your shell's `PATH` into `exec-path` at startup instead of writing absolute paths into `envoy-providers`.

### use-package with the vc keyword (Emacs 30 or newer)

```elisp
(use-package envoy
  :vc (:url "https://github.com/nick-maderight/envoy" :rev :newest))
```



### use-package with elpaca

```elisp
(use-package envoy
  :ensure (:host github :repo "nick-maderight/envoy"))
```

### From source

```elisp
(add-to-list 'load-path "/path/to/envoy")
(require 'envoy)
(require 'envoy-org)    ; for envoy-org-heading
(require 'envoy-tmux)   ; for envoy-tmux-org-heading
```

Envoy binds no keys until you ask.

```elisp
(envoy-org-setup-keys)    ; C-c C-x A runs without a terminal in Org and agenda buffers
(envoy-tmux-setup-keys)   ; C-c C-x A R P C O S run in tmux from Org and agenda buffers
```

Emacs reserves `C-c` followed by a letter for you, so your init can add `C-c x O` for OMP. Turning on `envoy-org-collect-mode` after `envoy-org` loads collects terminal reports for you.

Automatic report collection stays silent in the echo area and `*Messages*`. Run `M-x envoy-org-collect-reports` to collect manually and see its summary.

### Optional notmuch setup

I use notmuch for my email, this is up to you. 

```elisp
(autoload 'envoy-notmuch-delegate
          "/path/to/envoy/envoy-notmuch.el" nil t)
(autoload 'envoy-notmuch-delegate-dsh
          "/path/to/envoy/envoy-notmuch.el" nil t)
(with-eval-after-load 'notmuch-show
  (define-key notmuch-show-mode-map (kbd "C-c C-x O")
    #'envoy-notmuch-delegate)
  (define-key notmuch-show-mode-map (kbd "C-c C-x S")
    #'envoy-notmuch-delegate-dsh))
```

### Optional Dired setup

```elisp
(autoload 'envoy-dired-setup-keys "/path/to/envoy/envoy-dired.el" nil t)
(with-eval-after-load 'dired
  (envoy-dired-setup-keys))
```

`dired-preview` binds `C-c C-x` in its own minor-mode map, which shadows these keys while its preview is on. Unbind it there or move its hexl toggle to another key.

### Your own agents, models and wrappers

Providers ship with empty `:args`, `:setups` and `:models`, except for codex's paste setting in `:args` and dsh's two models in `:models`. The permissions an agent runs with, the wrapper that picks an account and the models a gateway offers belong to your machine, so they go in your init after the terminal module loads.

```elisp
(with-eval-after-load 'envoy-tmux
  (setq envoy-tmux-providers
        (mapcar (lambda (entry) (cons (car entry) (copy-sequence (cdr entry))))
                envoy-tmux-providers))
  (setf (plist-get (alist-get 'claude envoy-tmux-providers) :args)
        '("--permission-mode" "plan"))
  (setf (plist-get (alist-get 'pi envoy-tmux-providers) :setups)
        '("my-wrapper profile-a" "my-wrapper profile-b"))
  (setf (plist-get (alist-get 'reasonix envoy-tmux-providers) :models)
        '("provider/model-a" "provider/model-b")))
(setq envoy-org-spool-directory "~/.emacs.d/envoy-reports")
```

The `mapcar` copy gives your settings their own plists instead of editing the quoted default in place.

## Settings

| Setting | Meaning |
|----|----|
| `envoy-provider` | which agent, a key of `envoy-providers`: `'claude`, `'reasonix` or `'dsh` |
| `envoy-model` | a model to ask for, or nil for the agent's own |
| `envoy-deny-tools` | tools to deny, `'("Bash")` by default |
| `envoy-timeout` | seconds before a run is given up on, 600 |
| `envoy-environment-timeout` | seconds a provider's credential command may run before it is killed, 10 |
| `envoy-review-display` | `'diff` or `'none` |
| `envoy-rewrite-context-lines` | read-only context lines on each side, 3 |
| `envoy-rewrite-extra-instructions` | text added to every rewrite brief, or nil |
| `envoy-providers` | how to invoke each agent |
| `envoy-default-direction` | what a dispatch sends when the direction is left empty |

`M-x envoy-select-provider` switches the agent for the session. With `envoy-review-display` set to `'none` the change is already on disk, and undo works only while the buffer has the file open.

| Setting | Meaning |
|----|----|
| `envoy-org-done-keyword` | keyword after successful delivery and explicit completion of the whole subtree, `'done` for the heading's own |
| `envoy-org-active-keyword` | keyword during work or after an incomplete, missing, or invalid status, `"DOING"` |
| `envoy-org-drawer` | drawer the report goes in, `"ENVOY"` |
| `envoy-org-report-instructions` | whether the brief requests the optional report format. The status line is always required |
| `envoy-org-output-instructions` | whether the brief says where to put files |
| `envoy-org-outward-action-guard` | whether the brief says that only the harness's send confirmation authorizes a send |
| `envoy-org-spool-directory` | where a terminal's report waits to be collected |
| `envoy-org-collect-interval` | seconds between sweeps of the spool, 60 |

Setting `envoy-org-done-keyword` to nil disables the done transition. Setting `envoy-org-active-keyword` to nil preserves the current keyword while work is active or incomplete.

| Setting | Meaning |
|----|----|
| `envoy-tmux-provider` | which agent, `'claude`, `'reasonix`, `'pi`, `'codex`, `'omp` or `'dsh` |
| `envoy-tmux-providers` | how to start each agent in a window |
| `envoy-tmux-start-timeout` | seconds to wait for an agent before typing at it, 5 |
| `envoy-tmux-draw-timeout` | seconds to wait for an interface before a keystroke, 20 |
| `envoy-tmux-type-pause` | seconds between the pieces of a typed brief, 0.15 |
| `envoy-tmux-program` | the tmux executable |

## Adding another agent

An agent that runs non-interactively and prints a JSON result joins `envoy-providers` without touching any code.

```elisp
(add-to-list 'envoy-providers
             '(myagent
               :program "myagent"
               :name "My Agent"
               :args ("-p" "--output-format" "json")
               :edit-args ("--write")
               :deny-arg "--no-tools"
               :deny-separate t
               :model-arg "--model"))
```

The envelope requires `result` for the agent's closing message and `is_error` for whether it failed. `session_id`, `total_cost_usd` and `num_turns` are used when present. Set `:deny-arg` to nil for an agent with no such option, and Envoy warns instead of pretending.

An agent that reports some other way names its shape with `:format`. `json` is the envelope above and the default. `dsh` reads the closing event of a dsh headless run, where `reason` says how the turn ended. An agent that needs a credential Emacs does not hold names it in `:environment`, as a list of pairs holding a variable name and a command whose trimmed output becomes its value for that run.

A tmux agent joins `envoy-tmux-providers` by saying how the brief reaches it. `:prompt-arg` is a format string that gets the file holding the brief. `"@%s"` suits an agent that reads a file named on its command line, `"\"$(cat %s)\""` suits one that takes the prompt as an argument, and nil means Envoy types the brief into the terminal after the agent starts. `:opening-input` lists lines to type before the brief for an agent that only takes a setting through its own interface, and it requires `:prompt-arg` to be nil.

