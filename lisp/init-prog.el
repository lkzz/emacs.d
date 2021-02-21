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

(use-package dash-at-point
  :if is-mac-p
  :general ("C-c d" 'dash-at-point
            "C-c D" 'dash-at-point-with-docset))

;; https://github.com/honmaple/emacs-maple-imenu
(use-package maple-imenu
  :commands maple-imenu
  :straight (maple-imenu :host github :repo "honmaple/emacs-maple-imenu")
  :general (kevin/space-key-define "t i" 'maple-imenu)
  :config
  (setq maple-imenu-autoupdate t
        maple-imenu-width 35
        maple-imenu-indent 2
        maple-imenu-display-alist '((side . right) (slot . -1))))

(use-package nxml-mode
  :straight (:type built-in)
  :mode (("\\.xaml$" . xml-mode)))

(use-package toml-mode
  :mode (("\\.toml$" . toml-mode)))

(use-package bazel-mode
  :mode (("/BUILD\\(\\..*\\)?\\'" . bazel-mode)
         ("/WORKSPACE\\'" . bazel-mode)
         ("\\.bzl\\'" . bazel-mode))
  :init
  (add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t))))

(use-package protobuf-mode
  :straight (:host github :repo "emacsmirror/protobuf-mode")
  :diminish abbrev-mode ;; required in protobuf-mode
  :mode (("\\.proto$" . protobuf-mode))
  :init
  (defconst kevin/protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook (lambda () (c-add-style "my-style" kevin/protobuf-style t))))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode))
  :init
  (setq yaml-indent-offset 4))

(use-package editorconfig
  :diminish editorconfig-mode
  :hook (after-init . editorconfig-mode))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(provide 'init-prog)
;;; init-prog.el ends here
