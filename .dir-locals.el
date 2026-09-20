;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

;; envoy.el carries the package headers; the other three files are library
;; parts of the same package.  Without this, package-lint treats each file as a
;; package of its own and asks every one of them for its own Version header and
;; its own dependency list, and it reads the file name as the required symbol
;; prefix -- so `envoy-review-accept' in envoy-review.el is reported as wrongly
;; prefixed.
((emacs-lisp-mode . ((package-lint-main-file . "envoy.el")
                     (indent-tabs-mode . nil)
                     (fill-column . 79))))
