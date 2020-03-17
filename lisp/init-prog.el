;; init-prog.el --- Initialize prog configurations. -*- lexical-binding: t; -*-
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
;;; Code:

(use-package dash-at-point
  :if is-mac-p
  :bind (("C-c d" . dash-at-point)
         ("C-c D" . dash-at-point-with-docset)))

;; https://github.com/honmaple/emacs-maple-imenu
(use-package maple-imenu
  :load-path "site-lisp/emacs-maple-imenu"
  :commands maple-imenu
  :config
  (setq maple-imenu-autoupdate t
        maple-imenu-width 35
        maple-imenu-indent 2
        maple-imenu-display-alist '((side . right) (slot . -1))))

(use-package prog-mode
  :ensure nil
  :hook ((emacs-lisp-mode . global-prettify-symbols-mode)
         (emacs-lisp-mode . (lambda () (push '("<=" . ?≤) prettify-symbols-alist)))))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(use-package toml-mode
  :mode (("\\.toml$" . toml-mode)))

(use-package fish-mode
  :init
  (add-hook 'fish-mode-hook (lambda () (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package bazel-mode
  :mode (("/BUILD\\(\\..*\\)?\\'" . bazel-mode)
         ("/WORKSPACE\\'" . bazel-mode)
         ("\\.bzl\\'" . bazel-mode))
  :init
  (add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t))))

(use-package protobuf-mode
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

(use-package json-reformat
  :commands (json-reformat-region))

(use-package editorconfig
  :diminish editorconfig-mode
  :hook (after-init . editorconfig-mode))

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'")

(provide 'init-prog)
;;; init-prog.el ends here
