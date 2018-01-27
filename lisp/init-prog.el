;; init-prog.el --- Initialize prog configurations.
;;
;;; Commentary:
;;             Configurations for prog mode.
;;; Code:

(when sys/macp
    (use-package dash-at-point
    :bind (("\C-cD" . dash-at-point)
       ("\C-ce" . dash-at-point-with-docset))))

(defun kevin/prog-mode-hook ()
  "Custom config used in programming mode."
  ;; turn off 'nlinum-mode when there are more than 5000 lines
  (if (buffer-too-big-p) (nlinum-mode -1))
  ;; show trailing spaces in a programming mode
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'kevin/prog-mode-hook)

(use-package prog-mode
  :ensure nil
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
  :init (add-hook 'after-init-hook #'dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy)))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))


(use-package toml-mode:
  :ensure nil
  :mode (("\\.toml$" . toml-mode)))

(use-package quickrun
  :bind (("<f7>" . quickrun)
         ("C-c x" . quickrun)))

(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package vimrc-mode)

;; ;; New `conf-toml-mode' in Emacs26
;; (unless (fboundp 'conf-toml-mode)
;;   (use-package toml-mode))

(use-package editorconfig
  :diminish editorconfig-mode
  :init (add-hook 'after-init-hook #'editorconfig-mode))

;; New `bat-mode' in 25, only use `batch-mode' in 24.
(unless (fboundp 'bat-mode)
  (use-package batch-mode
    :mode (("\\.\\(cmd\\|bat\\)$" . batch-mode))))

(use-package fish-mode
  :init
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'fish_indent-before-save))))

(use-package swift-mode
  :config
  (with-eval-after-load 'flycheck
    (use-package flycheck-swift
      :init (flycheck-swift-setup))))

(use-package rust-mode
  :config (setq rust-format-on-save t))

(use-package robot-mode
  :ensure nil
  :load-path "site-lisp"
  :commands robot-mode
  :mode "\\.robot\\'")

(provide 'init-prog)
;;; init-prog.el ends here
