;; init-golang.el --- Initialize Golang configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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
;;; Code:

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (my/local-leader-define go-mode-map
    "i" '(nil :wk "import")
    "i a" 'go-import-add
    "i g" 'go-goto-imports
    "i r" 'go-remove-unused-imports
    "t" '(nil :wk "test")
    "t b" 'go-test-current-benchmark
    "t g" 'go-gen-test-dwim
    "t t" 'go-test-current-test
    "t f" 'go-test-current-file
    "t p" 'go-test-current-project
    "T" '(nil :wk "tag")
    "T a" 'go-tag-add
    "T r" 'go-tag-remove
    "x" '(nil :wk "run")
    "x x" 'go-run)

  (if (executable-find "goimports")
      (setq gofmt-command "goimports") ;; go install golang.org/x/tools/cmd/goimports@latest
    (message "command goimports not found"))

  ;; (if (executable-find "gofumpt")
  ;;     (setq gofmt-command "gofumpt") ;; go install mvdan.cc/gofumpt@latest
  ;;   (message "command gofumpt not found"))

  (add-hook 'before-save-hook #'gofmt-before-save)

  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GOBIN" "GO111MODULE" "GOPROXY")))
  (use-package go-tag
    :init (setq go-tag-args (list "-transform" "camelcase")))

  ;; Install: See https://github.com/golangci/golangci-lint#install
  (use-package flycheck-golangci-lint
    :if (executable-find "golangci-lint")
    :after flycheck
    :defines flycheck-disabled-checkers
    :init
    (setq flycheck-golangci-lint-tests t
          flycheck-golangci-lint-disable-linters '("unused" "staticcheck" "misspell" "errcheck"))
    :hook (go-mode . (lambda ()
                       ;; Remove default go flycheck-checkers except:go-build and go-test
                       (setq flycheck-disabled-checkers '(go-gofmt
                                                          go-golint
                                                          go-vet
                                                          go-errcheck
                                                          go-unconvert
                                                          go-staticcheck))
                       "Enable golangci-lint."
                       (flycheck-golangci-lint-setup)

                       ;; Make sure to only run golangci after go-build
                       ;; to ensure we show at least basic errors in the buffer
                       ;; when golangci fails. Make also sure to run go-test if possible.
                       ;; See #13580 for details
                       (flycheck-add-next-checker 'go-build '(warning . golangci-lint) t)
                       (flycheck-add-next-checker 'go-test '(warning . golangci-lint) t)

                       ;; Set basic checkers explicitly as flycheck will
                       ;; select the better golangci-lint automatically.
                       ;; However if it fails we require these as fallbacks.
                       (cond ((flycheck-may-use-checker 'go-test) (flycheck-select-checker 'go-test))
                             ((flycheck-may-use-checker 'go-build) (flycheck-select-checker 'go-build))))))

  (use-package go-gen-test)
  (use-package gotest
    :init (setq go-test-verbose t)))

(provide 'init-golang)
;;; init-golang ends here
