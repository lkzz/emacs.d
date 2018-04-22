;;; init-projectile.el --- Initialize projectile
;;; Commentary:
;;; Code:

;; (use-package projectile
;;   :ensure t
;;   :defer t
;;   :diminish projectile-mode "ⓟ"
;;   :commands (hydra-projectile-other-window/body hydra-projectile/body)
;;   :hook (after-init . projectile-mode)
;;   :init
;;   (progn
;;     (evil-leader/set-key "p" #'hydra-projectile/body)
;;     (defhydra hydra-projectile-other-window (:color teal)
;;       "projectile-other-window"
;;       ("f"  projectile-find-file-other-window        "file")
;;       ("g"  projectile-find-file-dwim-other-window   "file dwim")
;;       ("d"  projectile-find-dir-other-window         "dir")
;;       ("b"  projectile-switch-to-buffer-other-window "buffer")
;;       ("q"  nil                                      "cancel" :color blue))
;;     (defhydra hydra-projectile (:color teal
;;                                        :hint nil)
;;       "
;;    Find File            Search/Tags          Buffers                Cache
;;    ------------------------------------------------------------------------------------------
;;    _f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
;;    _r_: replace         _g_: update gtags      _b_: switch to buffer  _x_: remove known project
;;    _d_: dir             _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
;;    "
;;       ("a"   projectile-ag)
;;       ("b"   projectile-switch-to-buffer)
;;       ("c"   projectile-invalidate-cache)
;;       ("d"   projectile-find-dir)
;;       ("f"   projectile-find-file)
;;       ("g"   ggtags-update-tags)
;;       ("s-g" ggtags-update-tags)
;;       ("i"   projectile-ibuffer)
;;       ("K"   projectile-kill-buffers)
;;       ("s-k" projectile-kill-buffers)
;;       ("m"   projectile-multi-occur)
;;       ("o"   projectile-multi-occur)
;;       ("p"   helm-projectile)
;;       ("s"   projectile-switch-project)
;;       ("r"   projectile-replace )
;;       ("x"   projectile-remove-known-project)
;;       ("X"   projectile-cleanup-known-projects)
;;       ("z"   projectile-cache-current-file)
;;       ("`"   hydra-projectile-other-window/body "other window")
;;       ("q"   nil "cancel" :color blue)))
;;   :config
;;   (progn
;;     (setq projectile-known-projects-file
;;           (concat kevin/cache-directory "projectile-bookmarks.eld"))
;;     (setq projectile-cache-file
;;           (concat kevin/cache-directory "projectile.cache"))
;;     (setq projectile-sort-order 'recentf)
;;     (setq projectile-use-git-grep t)))


(use-package projectile
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
          (evil-leader/set-key
            "/"  'projectile-ag
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
            (projectile-global-mode)))

(provide 'init-projectile)
;;; init-projectile ends here
