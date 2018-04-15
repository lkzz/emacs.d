;;; init-projectile.el --- Initialize projectile
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode "ⓟ"
  :commands (hydra-projectile-other-window/body hydra-projectile/body)
  :hook (after-init . projectile-mode)
  :init
  (progn
    (evil-leader/set-key "p" #'hydra-projectile/body)
    (defhydra hydra-projectile-other-window (:color teal)
      "projectile-other-window"
      ("f"  projectile-find-file-other-window        "file")
      ("g"  projectile-find-file-dwim-other-window   "file dwim")
      ("d"  projectile-find-dir-other-window         "dir")
      ("b"  projectile-switch-to-buffer-other-window "buffer")
      ("q"  nil                                      "cancel" :color blue))
    (defhydra hydra-projectile (:color teal
                                       :hint nil)
      "
   Find File            Search/Tags          Buffers                Cache
   ------------------------------------------------------------------------------------------
   _f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
   _r_: replace         _g_: update gtags      _b_: switch to buffer  _x_: remove known project
   _d_: dir             _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
   "
      ("a"   projectile-ag)
      ("b"   projectile-switch-to-buffer)
      ("c"   projectile-invalidate-cache)
      ("d"   projectile-find-dir)
      ("f"   projectile-find-file)
      ("g"   ggtags-update-tags)
      ("s-g" ggtags-update-tags)
      ("i"   projectile-ibuffer)
      ("K"   projectile-kill-buffers)
      ("s-k" projectile-kill-buffers)
      ("m"   projectile-multi-occur)
      ("o"   projectile-multi-occur)
      ("p"   helm-projectile)
      ("s"   projectile-switch-project)
      ("r"   projectile-replace )
      ("x"   projectile-remove-known-project)
      ("X"   projectile-cleanup-known-projects)
      ("z"   projectile-cache-current-file)
      ("`"   hydra-projectile-other-window/body "other window")
      ("q"   nil "cancel" :color blue)))
  :config
  (progn
    (setq projectile-known-projects-file
          (concat kevin/cache-directory "projectile-bookmarks.eld"))
    (setq projectile-cache-file
          (concat kevin/cache-directory "projectile.cache"))
    (setq projectile-sort-order 'recentf)
    (setq projectile-use-git-grep t)))

(provide 'init-projectile)
;;; init-projectile ends here
