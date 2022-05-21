;;; init-lsp-bridge.el --- insert description here -*- lexical-binding: t -*-
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
;;; Code:

(when is-mac-p
  (setq lsp-bridge-python-command "/usr/local/bin/python3"))

(use-package lsp-bridge
  :straight (:host github :repo "manateelazycat/lsp-bridge" :files (:defaults "*"))
  :init
  (dolist (hook (list
                 'c-mode-hook
                 'c++-mode-hook
                 'java-mode-hook
                 'python-mode-hook
                 'ruby-mode-hook
                 'rust-mode-hook
                 'elixir-mode-hook
                 'go-mode-hook
                 'haskell-mode-hook
                 'haskell-literate-mode-hook
                 'dart-mode-hook
                 'scala-mode-hook
                 'typescript-mode-hook
                 'typescript-tsx-mode-hook
                 'js2-mode-hook
                 'js-mode-hook
                 'rjsx-mode-hook
                 'tuareg-mode-hook
                 'latex-mode-hook
                 'Tex-latex-mode-hook
                 'texmode-hook
                 'context-mode-hook
                 'texinfo-mode-hook
                 'bibtex-mode-hook
                 'clojure-mode-hook
                 'clojurec-mode-hook
                 'clojurescript-mode-hook
                 'clojurex-mode-hook
                 'sh-mode-hook
                 'web-mode-hook))
    (add-hook hook (lambda ()
                     (setq-local corfu-auto nil) ;; let lsp-bridge control when popup completion frame
                     (lsp-bridge-mode 1))))
  :config
  (setq lsp-bridge-enable-log t)
  (require 'lsp-bridge-orderless) ;; make lsp-bridge support fuzzy match, optional
  (require 'lsp-bridge-icon) ;; show icon for completion items, optional

  (general-evil-define-key 'normal lsp-bridge-mode-map
    "ga" 'xref-find-apropos
    "gd" 'lsp-bridge-find-def
    "K"  'lsp-bridge-lookup-documentation
    "gi" 'lsp-bridge-find-impl
    "gr" 'lsp-bridge-find-references))

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
