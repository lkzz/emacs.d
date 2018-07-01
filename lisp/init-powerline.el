;;; init-powerline.el --- modeline config for emacs.
;;; Commentary:
;;; Code:


(use-package powerline
  :config
  (progn
    (setq powerline-height 20)
    (setq powerline-default-separator 'arrow)
    (powerline-default-theme)))

(use-package powerline-evil
  :after (power evil))

(provide 'init-powerline)
