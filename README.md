# envoy

A new way to hand off work from Emacs to coding harnesses. I've tried gptel and other project and I believe this is the best way.


What it is in one sentence: you mark something (currently orgmode headers and notmuch emails) and with envoy, you send it off to a coding harness (e.g. Claude code and omp) inside tmux for execution. 

## Why?

This project closely follows the Unix philosophy of "do one thing well":

1. As an Emacs user, my daily routine consists of many "see something in Emacs, need to do it elsewhere" scenarios, and this pacakge is specifically for handing off work to coding harnesses. 
2. This package does not attempt to handle window persistence or management. We will let tmux manage that. 
3. This pacakge does not attempt to build control flows on top of raw LLM calls. We will let coding harnesses handle that, e.g. Claude code, omp (my favorite right now). 




## Demo

https://github.com/user-attachments/assets/0ee8d1e8-2b8e-4b8c-a2c9-e6c826917c33

A mail thread in notmuch goes to OMP. Mark the region, press `C-c C-x O`, pick the launch mode and the model, pick the tmux session, and the agent starts in that window with the brief already delivered. The file is also in the repository at [docs/envoy-demo.mp4](docs/envoy-demo.mp4).

## Commands

`envoy-rewrite` takes a marked region and an instruction from the minibuffer. The brief names the file and the line range and includes three read-only context lines on each side, so the agent can match the style around the selection. Only the selected lines may change.

`envoy-org-heading` treats an org heading as a task. The brief includes the whole subtree, every ancestor heading that is not datetree scaffolding, and the attachment directories of the heading and its ancestors. The agent works in the project that owns the file and writes its output to the heading's attachment directory. When it finishes, the keyword moves and a few sentences about what is now true of the files go into a drawer under the heading.

The ancestor walk is the point of the org commands. A task heading is almost never self-contained. The client, the budget, the deadline, the brief and what has already been ruled out sit two or three levels up. Sending the task alone sends the least informative part of it.

`envoy-tmux-org-heading` sends the same brief to an agent in a tmux window. Use it when the task is long enough that you will want to watch, or open-ended enough that the agent will want to ask you something.

`envoy-notmuch-delegate` works from a `notmuch-show-mode` buffer. It asks for an instruction and a tmux session and starts OMP there. An active region is the primary input and is copied verbatim. Without a region the whole thread is primary, including collapsed and filtered messages. OMP reads related threads through the notmuch CLI as secondary context. Quoted mail is untrusted context, your instruction wins, and mail work stays draft-only through `/email-draft`.

## Sending a heading to a terminal

Every dispatch asks which tmux session should receive the window. Envoy reads the live session names from the running tmux server and shows them in tmux's own order. A name that is not in the list starts a new session. There is no default, because you are the one who will attach to that session to read and answer the agent. An empty answer is rejected, and so is a name with `.` or `:` in it, because tmux would rewrite those to `_` and the session would be unreachable under the name you typed.

`envoy-tmux-setup-keys` binds `C-c C-x A` for Claude Code, `C-c C-x R` for reasonix, `C-c C-x P` for pi, `C-c C-x C` for codex and `C-c C-x O` for OMP in `org-mode`. A prefix argument asks for the provider's model or setup command first.

## In orgmode, the heading gets the answer

When the agent finishes, the keyword moves and the report goes into an `:ENVOY:` drawer under the heading, with a link to each file that appeared in the attachment directory. Envoy builds those links from the directory itself, so a file the agent named but never wrote gets no link, and a file it wrote but never mentioned does.


## Install

Envoy requires Emacs 27.1 or newer and at least one agent program on `exec-path`. The terminal commands also require tmux. `envoy-notmuch-delegate` requires OMP and the notmuch Emacs package.

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
(envoy-org-setup-keys)    ; C-c C-x A runs the agent without a terminal
(envoy-tmux-setup-keys)   ; C-c C-x A R P C O open a tmux window instead
```

The two setup functions overlap on `C-c C-x A`, and the later call owns that key. Emacs reserves `C-c` followed by a letter for you, so your init can add `C-c x O` for OMP. Turning on `envoy-org-collect-mode` after `envoy-org` loads collects terminal reports for you.

### Optional notmuch setup

I use notmuch for my email, this is up to you. 

```elisp
(autoload 'envoy-notmuch-delegate
          "/path/to/envoy/envoy-notmuch.el" nil t)
(with-eval-after-load 'notmuch-show
  (define-key notmuch-show-mode-map (kbd "C-c C-x O")
    #'envoy-notmuch-delegate))
```

### Your own agents, models and wrappers

Every provider ships with empty `:args`, `:setups` and `:models`. The permissions an agent runs with, the wrapper that picks an account and the models a gateway offers belong to your machine, so they go in your init after the terminal module loads.

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
        '("provider/model-a" "provider/model-b"))
  (setq envoy-tmux-omp-drive-program "omp-drive"))
(setq envoy-org-spool-directory "~/.emacs.d/envoy-reports")
```

The `mapcar` copy gives your settings their own plists instead of editing the quoted default in place.

## Settings

| Setting | Meaning |
|----|----|
| `envoy-provider` | which agent, `'claude` or `'reasonix` |
| `envoy-model` | a model to ask for, or nil for the agent's own |
| `envoy-deny-tools` | tools to deny, `'("Bash")` by default |
| `envoy-timeout` | seconds before a run is given up on, 600 |
| `envoy-review-display` | `'diff` or `'none` |
| `envoy-rewrite-context-lines` | read-only context lines on each side, 3 |
| `envoy-rewrite-extra-instructions` | text added to every rewrite brief, or nil |
| `envoy-providers` | how to invoke each agent |

`M-x envoy-select-provider` switches the agent for the session. With `envoy-review-display` set to `'none` the change is already on disk, and undo works only while the buffer has the file open.

| Setting | Meaning |
|----|----|
| `envoy-org-done-keyword` | keyword when the agent finishes, `'done` for the heading's own |
| `envoy-org-active-keyword` | keyword while it works, `"DOING"` |
| `envoy-org-drawer` | drawer the report goes in, `"ENVOY"` |
| `envoy-org-report-instructions` | whether the brief asks for the report in a set form |
| `envoy-org-output-instructions` | whether the brief says where to put files |
| `envoy-org-outward-action-guard` | whether the brief says it cannot authorize a send |
| `envoy-org-spool-directory` | where a terminal's report waits to be collected |
| `envoy-org-collect-interval` | seconds between sweeps of the spool, 60 |

Setting `envoy-org-done-keyword` to nil leaves the keyword alone, and `envoy-org-active-keyword` set to nil leaves it alone while the agent works.

| Setting | Meaning |
|----|----|
| `envoy-tmux-provider` | which agent, `'claude`, `'reasonix`, `'pi`, `'codex` or `'omp` |
| `envoy-tmux-providers` | how to start each agent in a window |
| `envoy-tmux-omp-drive-program` | the OMP Drive wrapper, `"omp-drive"` |
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

A tmux agent joins `envoy-tmux-providers` by saying how the brief reaches it. `:prompt-arg` is a format string that gets the file holding the brief. `"@%s"` suits an agent that reads a file named on its command line, `"\"$(cat %s)\""` suits one that takes the prompt as an argument, and nil means Envoy types the brief into the terminal after the agent starts. `:opening-input` lists lines to type before the brief for an agent that only takes a setting through its own interface, and it requires `:prompt-arg` to be nil.

