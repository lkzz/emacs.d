;; init-golang.el --- Initialize Golang configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;     go get -u github.com/gogo/protobuf/gogoproto
;;     go get -u github.com/golang/protobuf/proto
;;     go get -u github.com/gogo/protobuf/protoc-gen-gofast
;;     go get -u github.com/gogo/protobuf/protoc-gen-gogo
;;     go get -u github.com/golang/protobuf/protoc-gen-go
;;     go get -u github.com/jstemmer/gotags
;;     go get -u github.com/sourcegraph/go-langserver
;;     go get -u github.com/mdempsky/gocode
;;     go get -u github.com/godoctor/godoctor
;;     go get -u github.com/graphql-go/graphql
;;     go get -u github.com/zmb3/gogetdoc
;;     go get -u github.com/lukehoban/go-outline
;;     go get -u github.com/uudashr/gopkgs/cmd/gopkgs
;;     go get -u github.com/golang/lint/golint
;;     go get -u github.com/dougm/goflymake
;;     go get -u github.com/cweill/gotests/...
;;     go get -u github.com/hawkingrei/kazel
;;     go get -u github.com/smartystreets/goconvey
;;     go get -u github.com/golang/lint/golint
;;     go get -u github.com/sqs/goreturns
;;     go get -u github.com/rogpeppe/godef
;;     go get -u golang.org/x/tools/cmd/...

;;; Code:

(defun setup-go-mode-compile ()
  "Customize compile command to run go build"
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v")))

;; refer link: https://emacs-china.org/t/golang/6973
(defun kevin/go-auto-comment ()
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (let* ((imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (if current-prefix-arg
                                       (buffer-size)
                                     imenu-auto-rescan-maxout))
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items)))
    (cl-mapcan
     (lambda(item)
       (cl-mapcan
        (if (string= (car item) "func")
            'kevin/go-func-comment
          'kevin/go-type-comment)
        (cdr item)))
     items)))

(defun kevin/go-add-comment (func point)
  (save-excursion
    (goto-char point)
    (forward-line -1)
    (when (not (looking-at (concat "// " func)))
      (end-of-line) (newline-and-indent)
      (insert (concat "// " func " ..")))))

(defun kevin/go-func-comment (f)
  (let ((func (car f)))
    (if (and (string-prefix-p "(" func)
             (string-match "[)] \\(.*\\)[(]\\(.*\\)[)]\\(.*\\)$" func))
        (kevin/go-add-comment (match-string 1 func) (cdr f))
      (if (string-match "\\(.*\\)[(]\\(.*\\)[)]\\(.*\\)$" func)
          (kevin/go-add-comment (match-string 1 func) (cdr f))
        (kevin/go-add-comment (car f) (cdr f))))))

(defun kevin/go-type-comment (f)
  (kevin/go-add-comment (car f) (cdr f)))


(use-package go-mode
  :mode "\\.go\\'"
  :bind (:map go-mode-map
              ([remap xref-find-definitions] . godef-jump)
              ("C-c R" . go-remove-unused-imports)
              ("<f1>" . godoc-at-point))
  :config
  (kevin/define-jump-handlers go-mode godef-jump)
  (setq gofmt-command "goimports") ; use goimports instead of gofmt
  (add-hook 'go-mode-hook 'setup-go-mode-compile)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook #'kevin/revert-buffer-no-confirm)
  (kevin/declare-prefix-for-mode 'go-mode "mi" "imports")
  (kevin/set-leader-keys-for-major-mode 'go-mode
                                        "cj" 'godef-jump
                                        "ac" #'kevin/go-auto-comment
                                        "hh" 'godoc-at-point
                                        "ig" 'go-goto-imports
                                        "ia" 'go-import-add
                                        "ir" 'go-remove-unused-imports
                                        "eb" 'go-play-buffer
                                        "er" 'go-play-region
                                        "ed" 'go-download-play
                                        "ga" 'ff-find-other-file
                                        "gc" 'go-coverage)
  )

;; Run: M-x `go-projectile-install-tools'
(use-package go-projectile
  :after (go-mode projectile)
  :commands (go-projectile-mode go-projectile-switch-project)
  :hook ((go-mode . go-projectile-mode)
         (projectile-after-switch-project . go-projectile-switch-project)))

(use-package go-eldoc
  :after (go-mode eldoc)
  :commands (godoc-at-point)
  :hook (go-mode . go-eldoc-setup))

(use-package golint
  :after go-mode)

(use-package govet
  :after go-mode)

(use-package go-errcheck
  :after go-mode)

(use-package go-guru
  :after go-mode
  :commands (go-guru-describe go-guru-freevars go-guru-implements go-guru-peers
                              go-guru-referrers go-guru-definition go-guru-pointsto
                              go-guru-callstack go-guru-whicherrs go-guru-callers go-guru-callees
                              go-guru-set-scope)
  :init
  (kevin/declare-prefix-for-mode 'go-mode "mf" "guru")
  (kevin/set-leader-keys-for-major-mode 'go-mode
                                        "fd" 'go-guru-describe
                                        "ff" 'go-guru-freevars
                                        "fi" 'go-guru-implements
                                        "fc" 'go-guru-peers
                                        "fr" 'go-guru-referrers
                                        "fj" 'go-guru-definition
                                        "fp" 'go-guru-pointsto
                                        "fs" 'go-guru-callstack
                                        "fe" 'go-guru-whicherrs
                                        "f<" 'go-guru-callers
                                        "f>" 'go-guru-callees
                                        "fo" 'go-guru-set-scope))

(defun kevin/go-test-current-test-verbose()
  "Add -v flag to go test command."
  (interactive)
  (setq go-test-verbose t)
  (funcall 'go-test-current-test)
  (setq go-test-verbose nil))

(use-package gotest
  :after go-mode
  :config
  (kevin/declare-prefix-for-mode 'go-mode "mt" "test")
  (kevin/set-leader-keys-for-major-mode 'go-mode
                                        "tx" 'go-run
                                        "tb" 'go-test-current-benchmark
                                        "tt" 'go-test-current-test
                                        "tv" 'kevin/go-test-current-test-verbose
                                        "tm" 'go-test-current-file
                                        "tp" 'go-test-current-project))

(use-package go-imenu
  :after go-mode
  :config (add-hook 'go-mode-hook 'go-imenu-setup))

(use-package godoctor
  :after go-mode
  :config
  (kevin/declare-prefix-for-mode 'go-mode "mr" "refactoring")
  (kevin/set-leader-keys-for-major-mode 'go-mode
                                        "rn" 'godoctor-rename
                                        "re" 'godoctor-extract
                                        "rt" 'godoctor-toggle
                                        "rd" 'godoctor-godoc))

(use-package go-tag
  :after go-mode
  :config
  (kevin/set-leader-keys-for-major-mode 'go-mode
                                        "rf" 'go-tag-add
                                        "rF" 'go-tag-remove))

(use-package company-go
  :after (company go-mode)
  :init (cl-pushnew 'company-go company-backends)
  :config
  (setq company-go-show-annotation t))

(provide 'init-golang)
;;; init-golang ends here
