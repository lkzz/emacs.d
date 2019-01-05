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

(use-package imenu-list
  :defer t
  :ensure t
  :config
  (setq imenu-list-size     0.2)
  (setq imenu-list-position 'right)
  (setq imenu-list-focus-after-activation t))

(provide 'init-imenu)
;;; init-imenu.el ends here
