;;; init-imenu.el --- imenu config. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; with use-package
(use-package maple-imenu
  :straight (emacs-maple-imenu :host github :repo "honmaple/emacs-maple-imenu")
  :commands (maple-imenu)
  :init
  (kevin/set-leader-keys "ti" 'maple-imenu)
  :config
  (setq maple-imenu-autoupdate t
        maple-imenu-width 25
        maple-imenu-indent 2
        maple-imenu-display-alist '((side . right) (slot . -1))))

(provide 'init-imenu)
;;; init-imenu.el ends here
