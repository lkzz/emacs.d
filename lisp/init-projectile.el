;;; init-projectile.el --- Initialize projectile. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(use-package projectile
  :defer t
  :ensure t
  :diminish projectile-mode "ⓟ"
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-grep
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
  :init (progn
          (setq projectile-sort-order 'recentf
                projectile-cache-file (concat kevin/cache-directory
                                              "projectile.cache")
                projectile-known-projects-file (concat kevin/cache-directory
                                                       "projectile-bookmarks.eld"))
          (kevin/declare-prefix "p" "projectile")
          (kevin/set-leader-keys
           "p/"  'projectile-ag
           "p!" 'projectile-run-shell-command-in-root
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
           "pv" 'projectile-vc))
  :config (progn
            (setq projectile-completion-system 'ivy)
            (setq projectile-enable-caching t)
            (projectile-global-mode)))

(provide 'init-projectile)
;;; init-projectile ends here
