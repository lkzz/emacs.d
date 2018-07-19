;; init-prog.el --- Initialize prog configurations.
;;
;;; Commentary:
;;             Configurations for prog mode.
;;; Code:

(when sys/macp
  (use-package dash-at-point
    :bind (("\C-cD" . dash-at-point)
           ("\C-ce" . dash-at-point-with-docset))))

(use-package prog-mode
  :ensure nil
  :defer t
  :init
  ;; e.g. display “lambda” as “λ”
  (when (boundp 'global-prettify-symbols-mode)
    (add-hook 'after-init-hook #'global-prettify-symbols-mode)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (push '("<=" . ?≤) prettify-symbols-alist)))))

(use-package nxml-mode
  :ensure nil
  :defer t
  :mode (("\\.xaml$" . xml-mode)))

(use-package toml-mode
  :defer t
  :mode (("\\.toml$" . toml-mode)))

(use-package fish-mode
  :defer t
  :init
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'fish_indent-before-save))))

(use-package bazel-mode
  :defer t
  :mode (("/BUILD\\(\\..*\\)?\\'" . bazel-mode)
         ("/WORKSPACE\\'" . bazel-mode)
         ("\\.bzl\\'" . bazel-mode))
  :init
  (add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t))))

(use-package quickrun
  :defer t
  :bind (("<f7>" . quickrun)
         ("C-c x" . quickrun)))

(use-package json-reformat
  :ensure t
  :commands (json-reformat-region)
  :defer t)

(use-package editorconfig
  :defer t
  :diminish editorconfig-mode
  :hook (after-init . editorconfig-mode))


(provide 'init-prog)
;;; init-prog.el ends here
