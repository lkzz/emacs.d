;;; init-restore.el --- Initialize restore configurations.
;;; Commentary:
;;; Code:

;; Save and restore status
(use-package desktop
  :init (desktop-save-mode 1)
  :config
  ;; Restore frames into their original displays (if possible)
  (setq desktop-restore-in-current-display t)

  ;; Don't save/restore frames in tty
  (unless (display-graphic-p)
    (setq desktop-restore-frames nil)))

;; Persistent the scratch buffter
(use-package persistent-scratch
  :defer t
  :hook (after-init . persistent-scratch-setup-default)
  :config
  (setq persistent-scratch-save-file (concat kevin-cache-directory ".persistent-scratch")))

(provide 'init-restore)
;;; init-restore.el ends here
