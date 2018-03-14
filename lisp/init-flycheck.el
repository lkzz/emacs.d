;;; init-flycheck.el --- initialize flycheck
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :init
  (progn
    (setq flycheck-emacs-lisp-check-declare t)
    (setq flycheck-indication-mode 'right-fringe)
    (setq flycheck-emacs-lisp-load-path 'inherit))
  :config
  ;; Display Flycheck errors in GUI tooltips
  (use-package flycheck-pos-tip
    :ensure t
    :init (flycheck-pos-tip-mode 1)
    :config (setq flycheck-pos-tip-timeout 15))

  ;; Jump to and fix syntax errors via `avy'
  (use-package avy-flycheck
    :ensure t
    :init (avy-flycheck-setup))

  ;; Which colors the mode line according to the Flycheck state of the current buffer
  (use-package flycheck-color-mode-line
    :ensure t
    :init (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
