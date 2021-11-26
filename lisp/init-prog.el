;; init-prog.el --- Initialize prog configurations. -*- lexical-binding: t; -*-
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
;;; Code:

;; https://github.com/honmaple/emacs-maple-imenu
(use-package maple-imenu
  :commands maple-imenu
  :straight (maple-imenu :host github :repo "honmaple/emacs-maple-imenu")
  :general (my-space-leader-def "t i" 'maple-imenu)
  :config
  (setq maple-imenu-autoupdate t
        maple-imenu-width 35
        maple-imenu-indent 2
        maple-imenu-display-alist '((side . right) (slot . -1))))

(use-package nxml-mode
  :straight (:type built-in)
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.rss\\'" . nxml-mode))
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))


(use-package toml-mode
  :mode (("\\.toml$" . toml-mode)))

(use-package bazel-mode
  :mode (("/BUILD\\(\\..*\\)?\\'" . bazel-mode)
         ("/WORKSPACE\\'" . bazel-mode)
         ("\\.bzl\\'" . bazel-mode))
  :straight (:host github :repo "emacsmirror/bazel" :files (:defaults "*"))
  :init
  (add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t))))

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :straight (:host github :repo "emacsmirror/protobuf-mode" :files (:defaults "*"))
  :init
  (defconst kevin/protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook (lambda () (c-add-style "my-style" kevin/protobuf-style t))))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :init
  (setq yaml-indent-offset 4))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(use-package lua-mode
  :mode "\\.lua$"
  :hook (lua-mode . lsp-deferred)
  :init
  (setq lua-indent-level 2))

;; vimrc mode
(use-package vimrc-mode
  :mode ("/\\.?g?vimrc$"
         "\\.vim$"))

;; Cross-referencing commands
(use-package xref
  :ensure nil
  :general
  ('normal "g d" 'xref-find-definitions
           "g r" 'xref-find-references)
  :init
  (when (and (boundp 'xref-search-program) (executable-find "rg"))
    (setq xref-search-program 'ripgrep))
  (when is-emacs28-p
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read
          xref-show-definitions-function #'xref-show-definitions-completing-read))
  :hook ((xref-after-return xref-after-jump) . recenter))

;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ivy)
  :bind (("M-g j" . dumb-jump-go)
         ("M-g J" . dumb-jump-go-other-window)))

(provide 'init-prog)
;;; init-prog.el ends here
