;;; init-projectile.el --- projectile config. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2020  Kevin Leung
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

(use-package ripgrep :defer t)
(use-package projectile-ripgrep :defer t)

(use-package projectile
  :diminish projectile-mode "ⓟ"
  :commands (projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-replace-regexp
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :hook (after-init . projectile-mode)
  :general
  (kevin/space-key-define
    "p" '(nil :which-key "Projectile")
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
  (setq projectile-cache-file (concat kevin-cache-directory "projectile.cache")
        projectile-known-projects-file (concat kevin-cache-directory "projectile-bookmarks.eld")
        projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "build"))

  (provide 'init-projectile)
;;; init-projectile ends here
