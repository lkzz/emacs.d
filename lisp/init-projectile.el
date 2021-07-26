;;; init-projectile.el --- projectile config. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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
  :general
  (my-space-leader-def
    "p" '(nil :wk "projectile")
    "p !" 'projectile-run-shell-command-in-root
    "p &" 'projectile-run-async-shell-command-in-root
    "p %" 'projectile-replace-regexp
    "p /" 'projectile-ripgrep
    "p a" 'projectile-toggle-between-implementation-and-test
    "p b" 'projectile-switch-to-buffer
    "p c" 'projectile-compile-project
    "p d" 'projectile-find-dir
    "p D" 'projectile-dired
    "p f" 'projectile-find-file
    "p F" 'projectile-find-file-dwim
    "p g" 'projectile-find-tag
    "p G" 'projectile-regenerate-tags
    "p I" 'projectile-invalidate-cache
    "p k" 'projectile-kill-buffers
    "p p" 'projectile-switch-project
    "p r" 'projectile-recentf
    "p R" 'projectile-replace
    "p T" 'projectile-test-project
    "p v" 'projectile-vc)
  :config
  (setq projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-completion-system 'ivy
        projectile-globally-ignored-file-suffixes
        '(".dir" ".cmake" ".make" ".o" ".includecache" ".elc" ".internal"))
  (add-to-list 'projectile-globally-ignored-directories "build"))

(provide 'init-projectile)
;;; init-projectile ends here
