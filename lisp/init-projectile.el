;;; init-projectile.el --- projectile config. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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

(use-package projectile
  :diminish projectile-mode "ⓟ"
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-completion-system 'ivy
        projectile-globally-ignored-file-suffixes
        '(".dir" ".cmake" ".make" ".o" ".includecache" ".elc" ".internal"))
  (add-to-list 'projectile-globally-ignored-directories "build"))

(provide 'init-projectile)
;;; init-projectile ends here
