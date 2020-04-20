;; init-golang.el --- Initialize Golang configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2020  Kevin Leung
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
  :general
  (kevin/comma-key-define go-mode-map
    "i" '(nil :which-key "Import")
    "i a" 'go-import-add
    "i g" 'go-goto-imports
    "i r" 'go-remove-unused-imports
    "t" '(nil :which-key "Test")
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
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  (use-package go-tag
    :general (go-mode-map "C-c t t" 'go-tag-add
                          "C-c t T" 'go-tag-remove)
    :init (setq go-tag-args (list "-transform" "camelcase")))

  (use-package gotest
    :general (go-mode-map "C-c t v" 'go-test-current-test
                          "C-c t f" 'go-test-current-file
                          "C-c t p" 'go-test-current-project)
    :init
    (setq go-test-verbose t))

  (use-package go-gen-test
    :general (go-mode-map ("C-c t g" 'go-gen-test-dwim))))

(provide 'init-golang)
;;; init-golang ends here
