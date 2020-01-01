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
  :if kevin-mac-p
  :bind (("\C-cD" . dash-at-point)
         ("\C-ce" . dash-at-point-with-docset)))

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
  :ensure nil
  :diminish abbrev-mode ;; required in protobuf-mode
  :mode (("\\.proto$" . protobuf-mode))
  :init
  (defconst kevin/protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook (lambda () (c-add-style "my-style" kevin/protobuf-style t))))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)))

(use-package json-reformat
  :commands (json-reformat-region))

(use-package editorconfig
  :diminish editorconfig-mode
  :hook (after-init . editorconfig-mode))

(provide 'init-prog)
;;; init-prog.el ends here
