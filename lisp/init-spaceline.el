(use-package powerline
  :ensure t
  :config
  (setq powerline-height 25))

(use-package spaceline
  :ensure t
  :after powerline
  :init (setq powerline-default-separator 'arrow-fade)
  :config
  (progn
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    ;; (setq spaceline-responsive nil)
    ))

;; (use-package spaceline-colors
;;   :after spaceline-all-the-icons
;;   :config (advice-add 'load-theme :after 'spaceline-update-faces))

;; (use-package spaceline-all-the-icons
;;   :ensure t
;;   :after spaceline
;;   :config
;;   (setq spaceline-all-the-icons-icon-set-bookmark 'heart
;;         spaceline-all-the-icons-icon-set-modified 'toggle
;;         spaceline-all-the-icons-icon-set-dedicated 'pin
;;         ;; spaceline-all-the-icons-separator-type 'none
;;         spaceline-all-the-icons-icon-set-flycheck-slim 'dots
;;         spaceline-all-the-icons-flycheck-alternate t
;;         spaceline-all-the-icons-highlight-file-name t
;;         spaceline-all-the-icons-hide-long-buffer-path t)
;;   (spaceline-toggle-all-the-icons-bookmark-on)
;;   (spaceline-toggle-all-the-icons-dedicated-on)
;;   (spaceline-toggle-all-the-icons-fullscreen-on)
;;   (spaceline-toggle-all-the-icons-buffer-position-on)
;;   (spaceline-all-the-icons--setup-package-updates)
;;   (spaceline-all-the-icons--setup-paradox)
;;   (spaceline-all-the-icons--setup-neotree)
;;   (spaceline-all-the-icons-theme))

(provide 'init-spaceline)
;;; init-spaceline ends here
