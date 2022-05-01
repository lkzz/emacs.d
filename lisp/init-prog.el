;; init-prog.el --- Initialize prog configurations. -*- lexical-binding: t; -*-
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
;;; Code:

;; https://github.com/honmaple/emacs-maple-imenu
(use-package maple-imenu
  :commands maple-imenu
  :straight (maple-imenu :host github :repo "honmaple/emacs-maple-imenu")
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
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook (lambda () (c-add-style "my-style" kevin/protobuf-style t))))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :init
  (setq yaml-indent-offset 2))

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

(use-package xref
  :ensure nil
  :init
  (when (and (boundp 'xref-search-program) (executable-find "rg"))
    (setq xref-search-program 'ripgrep))
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook #'xref-after-jump-hook #'recenter)
  (add-hook #'xref-after-jump-hook #'better-jumper-set-jump)
  (add-hook #'xref-after-return-hook #'recenter)
  (add-hook #'xref-after-return-hook #'better-jumper-set-jump)
  :config
  (general-def 'normal
    "gr" #'xref-find-references
    "gd" #'xref-find-definitions))

(provide 'init-prog)
;;; init-prog.el ends here
