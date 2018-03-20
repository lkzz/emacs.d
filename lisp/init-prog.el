;; init-prog.el --- Initialize prog configurations.
;;
;;; Commentary:
;;             Configurations for prog mode.
;;; Code:

(when sys/macp
  (use-package dash-at-point
    :bind (("\C-cD" . dash-at-point)
           ("\C-ce" . dash-at-point-with-docset))))

(add-hook 'prog-mode-hook
          (lambda ()
            (if (kevin/buffer-too-big-p) (nlinum-mode -1))))

(use-package prog-mode
  :ensure nil
  :defer t
  :init
  ;; Prettify Symbols
  ;; e.g. display “lambda” as “λ”
  (when (boundp 'global-prettify-symbols-mode)
    (add-hook 'after-init-hook #'global-prettify-symbols-mode)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (push '("<=" . ?≤) prettify-symbols-alist)))))

;; Jump to definition via `ag'/`rg'/`grep'
(use-package dumb-jump
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook #'dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy)))

(use-package nxml-mode
  :ensure nil
  :defer t
  :mode (("\\.xaml$" . xml-mode)))


(use-package toml-mode
  :ensure t
  :defer t
  :mode (("\\.toml$" . toml-mode)))

(use-package quickrun
  :ensure t
  :bind (("<f7>" . quickrun)
         ("C-c x" . quickrun)))

(use-package editorconfig
  :ensure t
  :defer t
  :diminish editorconfig-mode
  :init (add-hook 'after-init-hook #'editorconfig-mode))

;; New `bat-mode' in 25, only use `batch-mode' in 24.
(unless (fboundp 'bat-mode)
  (use-package batch-mode
    :mode (("\\.\\(cmd\\|bat\\)$" . batch-mode))))

(use-package fish-mode
  :ensure t
  :init
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'fish_indent-before-save))))

(use-package swift-mode
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (use-package flycheck-swift
      :ensure t
      :init (flycheck-swift-setup))))

(use-package rust-mode
  :ensure t
  :config (setq rust-format-on-save t))

(use-package robot-mode
  :ensure nil
  :load-path "site-lisp"
  :commands robot-mode
  :mode "\\.robot\\'")

(provide 'init-prog)
;;; init-prog.el ends here
