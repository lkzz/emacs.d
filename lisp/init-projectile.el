;;; init-projectile.el --- projectile config. -*- lexical-binding: t; -*-
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

(use-package ripgrep
  :defer t)

(use-package projectile-ripgrep
  :defer t
  :init (kevin/set-leader-keys "p/" 'projectile-ripgrep))

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
  :custom
  (projectile-cache-file (concat kevin-cache-directory "projectile.cache"))
  (projectile-known-projects-file (concat kevin-cache-directory "projectile-bookmarks.eld"))
  :init
  (kevin/declare-prefix "p" "projectile")
  (kevin/set-leader-keys "p!" 'projectile-run-shell-command-in-root
                         "p&" 'projectile-run-async-shell-command-in-root
                         "p%" 'projectile-replace-regexp
                         "pa" 'projectile-toggle-between-implementation-and-test
                         "pb" 'projectile-switch-to-buffer
                         "pc" 'projectile-compile-project
                         "pd" 'projectile-find-dir
                         "pD" 'projectile-dired
                         "pf" 'projectile-find-file
                         "pF" 'projectile-find-file-dwim
                         "pg" 'projectile-find-tag
                         "pG" 'projectile-regenerate-tags
                         "pI" 'projectile-invalidate-cache
                         "pk" 'projectile-kill-buffers
                         "pp" 'projectile-switch-project
                         "pr" 'projectile-recentf
                         "pR" 'projectile-replace
                         "pT" 'projectile-test-project
                         "pv" 'projectile-vc)
  :config
  (setq projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-completion-system 'ivy)
  (projectile-global-mode))

  (provide 'init-projectile)
;;; init-projectile ends here
