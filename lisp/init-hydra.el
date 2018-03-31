;;; init-hydra.el --- Initialize hydra configurations.
;;; Commentary:
;;; Code:

(use-package hydra
  :ensure t)

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
   _r_: recent file     _g_: update gtags      _b_: switch to buffer  _x_: remove known project
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
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))
(evil-leader/set-key "p" 'hydra-projectile/body)


;; (defhydra hydra-multiple-cursors (:hint nil)
;;   "
;;      ^Up^            ^Down^        ^Other^
;;     ----------------------------------------------
;;     [_p_]   Previous    [_n_]   Next    [_l_] Edit lines
;;     [_P_]   Skip        [_N_]   Skip    [_a_] Mark all
;;     [_M-p_] Unmark      [_M-n_] Unmark  [_r_] Mark by regexp
;;     ^ ^                 ^ ^             [_q_] Quit
;;     "
;;   ("l" mc/edit-lines :exit t)
;;   ("a" mc/mark-all-like-this :exit t)
;;   ("n" mc/mark-next-like-this)
;;   ("N" mc/skip-to-next-like-this)
;;   ("M-n" mc/unmark-next-like-this)
;;   ("p" mc/mark-previous-like-this)
;;   ("P" mc/skip-to-previous-like-this)
;;   ("M-p" mc/unmark-previous-like-this)
;;   ("r" mc/mark-all-in-region-regexp :exit t)
;;   ("q" nil))


(provide 'init-hydra)
;;; init-hydra ends here
