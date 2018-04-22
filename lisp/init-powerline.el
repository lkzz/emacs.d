;;; init-powerline.el --- modeline config for emacs.
;;; Commentary:
;;; Code:


(use-package powerline
  :ensure t
  :config
  (progn
    (setq powerline-height 20)
    (setq powerline-default-separator 'arrow)
    (powerline-default-theme)))

(use-package powerline-evil
  :ensure t
  :after (power evil))

(provide 'init-powerline)
;;; init-powerline.el ends here
