;; init-prog.el --- Initialize prog configurations. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;             Configurations for prog mode.
;;; Code:

(when kevin-mac-p
  (use-package dash-at-point
    :defer t
    :bind (("\C-cD" . dash-at-point)
           ("\C-ce" . dash-at-point-with-docset))))

(use-package prog-mode
  :defer t
  :ensure nil
  :init
  ;; e.g. display “lambda” as “λ”
  (when (boundp 'global-prettify-symbols-mode)
    (add-hook 'after-init-hook #'global-prettify-symbols-mode)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (push '("<=" . ?≤) prettify-symbols-alist)))))

(use-package nxml-mode
  :defer t
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(use-package toml-mode
  :defer t
  :ensure t
  :mode (("\\.toml$" . toml-mode)))

(use-package fish-mode
  :defer t
  :ensure t
  :init
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'fish_indent-before-save))))

(use-package bazel-mode
  :defer t
  :ensure t
  :mode (("/BUILD\\(\\..*\\)?\\'" . bazel-mode)
         ("/WORKSPACE\\'" . bazel-mode)
         ("\\.bzl\\'" . bazel-mode))
  :init
  (add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t))))

(use-package protobuf-mode
  :defer t
  :ensure nil
  :diminish abbrev-mode ;; required in protobuf-mode
  :mode (("\\.proto$" . protobuf-mode))
  :init
  (progn
    (defconst kevin/protobuf-style
      '((c-basic-offset . 4)
        (indent-tabs-mode . nil)))
    (add-hook 'protobuf-mode-hook
              (lambda () (c-add-style "my-style" kevin/protobuf-style t)))))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode (("\\.yml\\'" . yaml-mode)))

(use-package quickrun
  :defer t
  :ensure t
  :bind (("<f7>" . quickrun)
         ("C-c x" . quickrun)))

(use-package json-reformat
  :defer t
  :ensure t
  :commands (json-reformat-region))

(use-package editorconfig
  :defer t
  :ensure t
  :diminish editorconfig-mode
  :hook (after-init . editorconfig-mode))


(provide 'init-prog)
;;; init-prog.el ends here
