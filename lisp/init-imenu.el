;;; init-imenu.el --- imenu config. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(use-package imenu-list
  :defer t
  :ensure t
  :config
  (progn
    (setq imenu-list-size     0.2)
    (setq imenu-list-position 'right)
    (setq imenu-list-focus-after-activation t)))

(provide 'init-imenu)
;;; init-imenu.el ends here
