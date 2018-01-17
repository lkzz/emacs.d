;;; init-whitespace.el --- whitespace mode config
;;; Commentary:
;;; Code:

;; 显示 whetespace
(whitespace-mode)
(setq-default whitespace-style '(face tabs trailing tab-mark))

;; 自动 trailing whitespace
(setq-default show-trailqing-whitespace t)

(defun no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'no-trailing-whitespace))

(require-package 'whitespace-cleanup-mode)

(add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
(global-set-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
