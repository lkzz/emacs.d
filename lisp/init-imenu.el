;;; init-imenu.el --- imenu config.
;;; Commentary:
;;; Code:

(use-package imenu-list
  :config
  (progn
    (setq imenu-list-size     0.2)
    (setq imenu-list-position 'right)
    (setq imenu-list-focus-after-activation t)
    ))

(provide 'init-imenu)
;;; init-imenu.el ends here
