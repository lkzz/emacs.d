
;;; Code:

(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

;; 当窗口小于3时，隐藏窗口编号
(setq-default switch-window-threshold 2)
;; after this many seconds, cancel the window switching
(setq-default switch-window-timeout nil)

;; I want to select minibuffer with label "z"
(setq-default switch-window-minibuffer-shortcut ?z)

;; window auto resize
(defun kevin/toggle-golden-ratio ()
  "Turn on/off auto resize window."
  (interactive)
  (setq switch-window-default-window-size 0.618) ;61.8% of frame size
  (if switch-window-auto-resize-window
      (progn
        (message "turn off auto resize window")
        (setq switch-window-auto-resize-window nil)
        (setq switch-window-mouse-auto-resize-window nil))
    (progn
      (message "turn on auto resize window")
      (setq switch-window-auto-resize-window t)
      (setq switch-window-mouse-auto-resize-window t)) ;auto resize when switch window with mouse
      ))
(global-set-key (kbd "C-c t g") 'kevin/toggle-gloden-ratio)

(window-numbering-mode t)

(use-package dimmer)

(provide 'init-windows)
;;; init-windows ends here
