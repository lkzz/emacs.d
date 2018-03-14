;;; init-persp-mode.el --- Initialize persp mode
;;; Commentary:
;;; Code:

(use-package persp-mode
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-name "nil"
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (concat user-emacs-directory "workspaces/")
        persp-set-last-persp-for-new-frames nil
        persp-switch-to-added-buffer nil
        persp-remove-buffers-from-nil-persp-behaviour nil
        ;; Don't restore winconf on new frames
        persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override 'auto-temp
        ;; Auto-load on startup
        persp-auto-resume-time 3
        ;; auto-save on kill
        persp-auto-save-opt (if noninteractive 0 1))
  )

(provide 'init-persp-mode)
;;; init-persp-mode.el ends here
