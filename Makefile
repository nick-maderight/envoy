# Makefile for envoy.
#
# Plain make, on purpose.  eldev and cask are both good, but each is one more
# thing to install before a contributor can run the tests, and this package
# needs nothing but Emacs and one of the two agent programs.
#
#   make test      run the ERT suite
#   make compile   byte-compile, treating every warning as an error
#   make lint      package-lint and checkdoc
#   make check     all of the above, which is what CI runs
#   make clean     remove .elc files

EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch -L .
TEST_TIMEOUT ?= 300
TEST_TIMEOUT_COMMAND := $(shell command -v timeout 2>/dev/null || command -v gtimeout 2>/dev/null)
TEST_TIMEOUT_NON_DIGITS := $(subst 0,,$(subst 1,,$(subst 2,,$(subst 3,,$(subst 4,,$(subst 5,,$(subst 6,,$(subst 7,,$(subst 8,,$(subst 9,,$(TEST_TIMEOUT)))))))))))

# Every --eval form below is written on one line, deliberately.
#
# A backslash continuation inside the single-quoted argument does not work: the
# shell cannot consume a backslash-newline inside single quotes, so the
# backslash reaches Emacs, whose reader takes `\' followed by a newline as an
# escaped character and yields a symbol whose name is a newline.  Evaluating
# that symbol signals `void-variable \' and Emacs exits 255.
#
# GNU make 3.81, which macOS still ships, collapses the continuation before the
# shell sees it; GNU make 4.x, which every Linux distribution ships, passes it
# through unchanged.  So a Makefile written that way passes locally and fails on
# every CI runner.  One line each keeps both versions honest.

# envoy.el must come after the two files it requires, then envoy-org.el,
# envoy-tmux.el, and the optional notmuch adapter: byte-compiling a file
# compiles what it requires as a side effect, without these warning settings.
SRC  = envoy-process.el envoy-review.el envoy.el envoy-org.el envoy-tmux.el envoy-notmuch.el
TEST = test/envoy-test.el
ELC  = $(SRC:.el=.elc) $(TEST:.el=.elc)
ifeq ($(strip $(TEST_TIMEOUT)),)
$(error TEST_TIMEOUT must be a non-negative integer)
endif
ifneq (x$(TEST_TIMEOUT_NON_DIGITS)x,xx)
$(error TEST_TIMEOUT must be a non-negative integer)
endif
ifeq ($(strip $(TEST)),)
$(error TEST must name a test file)
endif
export TEST_TIMEOUT TEST TEST_TIMEOUT_COMMAND

.PHONY: all check test compile lint lint-package lint-checkdoc clean help

all: check

check: compile test lint

# `timeout 0` means no limit, but `sleep 0` ends immediately and the
# watchdog kills the fallback suite. The fallback follows `timeout` and runs
# the suite directly for zero.
test:
	@if [ -n "$$TEST_TIMEOUT_COMMAND" ]; then \
	  "$$TEST_TIMEOUT_COMMAND" "$$TEST_TIMEOUT" $(BATCH) --eval '(setq load-prefer-newer t)' -l "$$TEST" -f ert-run-tests-batch-and-exit; \
	  status=$$?; \
	else \
	  if [ "$$TEST_TIMEOUT" -eq 0 ]; then \
	    $(BATCH) --eval '(setq load-prefer-newer t)' -l "$$TEST" -f ert-run-tests-batch-and-exit; \
	    status=$$?; \
	  else \
	    test_pid=; \
	    watchdog_pid=; \
	    trap 'if [ -n "$$test_pid" ]; then kill -TERM "$$test_pid" 2>/dev/null; fi; if [ -n "$$watchdog_pid" ]; then kill -TERM "$$watchdog_pid" 2>/dev/null; fi; exit 130' INT TERM; \
	    $(BATCH) --eval '(setq load-prefer-newer t)' -l "$$TEST" -f ert-run-tests-batch-and-exit & \
	    test_pid=$$!; \
	    ( \
	      trap 'if [ -n "$$sleep_pid" ]; then kill -TERM "$$sleep_pid" 2>/dev/null; wait "$$sleep_pid" 2>/dev/null; fi; exit 125' INT TERM; \
	      sleep "$$TEST_TIMEOUT" & \
	      sleep_pid=$$!; \
	      wait "$$sleep_pid"; \
	      kill -TERM "$$test_pid" 2>/dev/null || :; \
	      kill -KILL "$$test_pid" 2>/dev/null || :; \
	      exit 124 \
	    ) & \
	    watchdog_pid=$$!; \
	    wait "$$test_pid"; \
	    test_status=$$?; \
	    kill -TERM "$$watchdog_pid" 2>/dev/null || :; \
	    wait "$$watchdog_pid" 2>/dev/null; \
	    watchdog_status=$$?; \
	    trap - INT TERM; \
	    if [ "$$watchdog_status" -eq 124 ]; then \
	      status=124; \
	    else \
	      status="$$test_status"; \
	    fi; \
	  fi; \
	fi; \
	if [ "$$status" -eq 124 ]; then \
	  echo "make test: timed out after $$TEST_TIMEOUT seconds" >&2; \
	fi; \
	exit "$$status"

# The file names are passed as arguments and read from `command-line-args-left'
# rather than spliced into a quoted list, which would make them symbols.
#
# `byte-compile-error-on-warn' has to be set after bytecomp is loaded.  Binding
# it with `let' on the command line fails: at that point it is still an
# ordinary lexical variable, and defining it as dynamic afterwards is an error.
compile:
	$(BATCH) --eval '(progn (require (quote bytecomp)) (setq byte-compile-error-on-warn t) (dolist (f command-line-args-left) (unless (byte-compile-file f) (kill-emacs 1))))' $(SRC) $(TEST)

lint: lint-package lint-checkdoc

# package-lint is fetched into a throwaway directory rather than the real
# package directory, so linting never disturbs the user's own installation.
# .dir-locals.el names envoy.el as the main file; package-lint reads it because
# it calls `emacs-lisp-mode' on a buffer that already has a file name.
lint-package:
	$(BATCH) --eval '(progn (setq package-user-dir "/tmp/envoy-lint-elpa") (require (quote package)) (add-to-list (quote package-archives) (quote ("melpa" . "https://melpa.org/packages/")) t) (package-initialize) (unless (package-installed-p (quote package-lint)) (package-refresh-contents) (package-install (quote package-lint))) (require (quote package-lint)))' -f package-lint-batch-and-exit $(SRC)

# checkdoc always exits 0 and writes its findings to a buffer, so a naive batch
# run is silent whether the files are clean or not.  The exit status has to be
# derived from that buffer.
lint-checkdoc:
	$(BATCH) --eval '(progn (require (quote checkdoc)) (dolist (f command-line-args-left) (checkdoc-file f)) (let ((buf (get-buffer "*Warnings*"))) (when (and buf (> (buffer-size buf) 0)) (kill-emacs 1))))' $(SRC) $(TEST)

clean:
	rm -f $(ELC)

help:
	@echo "make test | compile | lint | check | clean"
