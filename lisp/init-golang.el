;; init-golang.el --- Initialize Golang configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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
  :hook (go-mode . lsp-deferred)
  :general
  (kevin/comma-key-define go-mode-map
    "i" '(nil :wk "Import")
    "i a" 'go-import-add
    "i g" 'go-goto-imports
    "i r" 'go-remove-unused-imports
    "t" '(nil :wk "Test")
    "t x" 'go-run
    "t b" 'go-test-current-benchmark
    "t t" 'go-test-current-test
    "t f" 'go-test-current-file
    "t p" 'go-test-current-project)
  (go-mode-map "C-c i a" 'go-import-add
               "C-c i g" 'go-goto-imports
               "C-c i r" 'go-remove-unused-imports
               "C-c r n" 'go-run)
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (setq-local lsp-diagnostics-provider 'flycheck))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  (use-package go-tag
    :general (go-mode-map "C-c t t" 'go-tag-add
                          "C-c t T" 'go-tag-remove)
    :init (setq go-tag-args (list "-transform" "camelcase")))

  ;; Install: See https://github.com/golangci/golangci-lint#install
  (use-package flycheck-golangci-lint
    :if (executable-find "golangci-lint")
    :after flycheck
    :defines flycheck-disabled-checkers
    :hook (go-mode . (lambda ()
                       "Enable golangci-lint."
                       (setq flycheck-disabled-checkers '(go-gofmt
                                                          go-golint
                                                          go-vet
                                                          go-errcheck
                                                          go-staticcheck
                                                          go-unconvert))
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

  (use-package go-gen-test
    :general (go-mode-map "C-c t g" 'go-gen-test-dwim))

  (use-package gotest
    :general (go-mode-map "C-c t v" 'go-test-current-test
                          "C-c t f" 'go-test-current-file
                          "C-c t p" 'go-test-current-project)
    :init (setq go-test-verbose t)))

(provide 'init-golang)
;;; init-golang ends here
