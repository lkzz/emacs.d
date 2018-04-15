;;; init-which-key.el --- which-key 设置
;;; Commentary:
;;; Code:

(use-package which-key
  :ensure t
  :demand t
  :diminish which-key-mode "ⓦ"
  :hook (after-init . which-key-mode)
  :config
  (progn
    (setq which-key-idle-delay 0.4)
    (setq which-key-side-window-max-width 0.33)
    (setq which-key-side-window-max-height 0.25)
    (setq which-key-allow-imprecise-window-fit t) ; performance
    (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
    (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
    (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
    (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
    ))

(provide 'init-which-key)
;;; init-which-key ends here
