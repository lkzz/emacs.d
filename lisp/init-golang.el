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
;;     go get -u -v github.com/golangci/golangci-lint/cmd/golangci-lint

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
  :mode ("\\.go\\'" . go-mode)
  :bind (:map go-mode-map
              ([remap xref-find-definitions] . godef-jump)
              ("C-c R" . go-remove-unused-imports)
              ("<f1>" . godoc-at-point))
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  (kevin/define-jump-handlers go-mode godef-jump)
  (setq gofmt-command "goimports") ; use goimports instead of gofmt
  (add-hook 'go-mode-hook 'setup-go-mode-compile)
  (add-hook 'before-save-hook #'gofmt-before-save)
  ;; (make-local-variable 'after-save-hook)
  ;; (add-hook 'after-save-hook #'kevin/revert-buffer-no-confirm)
  (kevin/declare-prefix-for-major-mode 'go-mode "i" "import")
  (kevin/declare-prefix-for-major-mode 'go-mode "g" "jump")
  (kevin/declare-prefix-for-major-mode 'go-mode "h" "help")
  (kevin/declare-prefix-for-major-mode 'go-mode "a" "comment")
  (kevin/declare-prefix-for-major-mode 'go-mode "e" "goplay")
  (kevin/set-leader-keys-for-major-mode
    :keymaps 'go-mode-map
    "ac" #'kevin/go-auto-comment
    "hh" 'godoc-at-point
    "ig" 'go-goto-imports
    "ia" 'go-import-add
    "ir" 'go-remove-unused-imports
    "eb" 'go-play-buffer
    "er" 'go-play-region
    "ed" 'go-download-play
    "ga" 'ff-find-other-file
    "gj" 'godef-jump
    "gc" 'go-coverage))

;; Install: go get -u github.com/golangci/golangci-lint/cmd/golangci-lint
(use-package flycheck-golangci-lint
  :disabled
  :if (executable-find "golangci-lint")
  :after flycheck
  :defines flycheck-disabled-checkers
  :hook (go-mode . (lambda ()
                     "Enable golangci-lint."
                     (setq flycheck-disabled-checkers '(go-gofmt
                                                        go-golint
                                                        go-vet
                                                        go-build
                                                        go-test
                                                        go-errcheck))
                     (flycheck-golangci-lint-setup))))

(use-package go-guru
  :after go-mode
  :commands (go-guru-describe go-guru-freevars go-guru-implements go-guru-peers
                              go-guru-referrers go-guru-definition go-guru-pointsto
                              go-guru-callstack go-guru-whicherrs go-guru-callers go-guru-callees
                              go-guru-set-scope)
  :config
  (kevin/declare-prefix-for-major-mode 'go-mode "f" "guru")
  (kevin/set-leader-keys-for-major-mode
    :keymaps 'go-mode-map
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
  (kevin/declare-prefix-for-major-mode 'go-mode "t" "test")
  (kevin/set-leader-keys-for-major-mode
    :keymaps 'go-mode-map
    "tx" 'go-run
    "tb" 'go-test-current-benchmark
    "tt" 'go-test-current-test
    "tv" 'kevin/go-test-current-test-verbose
    "tm" 'go-test-current-file
    "tp" 'go-test-current-project))

(use-package godoctor
  :after go-mode
  :config
  (kevin/declare-prefix-for-major-mode 'go-mode "r" "refactoring")
  (kevin/set-leader-keys-for-major-mode
    :keymaps 'go-mode-map
    "rn" 'godoctor-rename
    "re" 'godoctor-extract
    "rt" 'godoctor-toggle
    "rd" 'godoctor-godoc))

(use-package go-tag
  :after go-mode
  :config
  (kevin/set-leader-keys-for-major-mode
    :keymaps 'go-mode-map
    "rf" 'go-tag-add
    "rF" 'go-tag-remove))

(use-package company-go
  :disabled
  :after (company go-mode)
  :custom
  (company-go-gocode-args '("-builtin" "-unimported-packages" "-cache" "-fallback-to-source"))
  :config
  (setq company-go-show-annotation t)
  (kevin/add-company-backend :backend company-go :mode go-mode))

(provide 'init-golang)
;;; init-golang ends here
