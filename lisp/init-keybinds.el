;;; init-keybinds.el --- personal keybinds,should be required finally.
;;; Commentary:
;;; Code:


;; bookmark
(evil-leader/set-key
  "ob" nil
  "obs" 'bookmark-set
  "obr" 'bookmark-rename
  "obd" 'bookmark-delete
  "obj" 'counsel-bookmark
  "obl" 'bookmark-bmenu-list)

;; toggle
(evil-leader/set-key
  "ot" nil
  "otM" 'toggle-major-mode
  "otb" 'toggle-scroll-bar
  "otw" 'toggle-word-wrap
  "otm" 'toggle-frame-maximized
  "otf" 'toggle-frame-fullscreen
  "otg" 'golden-ratio-mode)

;; open applications with from emacs
(evil-leader/set-key
  "a" nil
  "ai" #'kevin/open-iterm ;; open item2
  "aw" #'kevin/open-wechat ; open WeChat
  "ay" #'kevin/open-youdao ; open youdao dictionary
  )

;; misc related keybindings
(evil-leader/set-key
  "=" 'text-scale-increase
  "-" 'text-scale-decrease
  "'" 'shell-pop
  "hd" 'describe-function
  "hf" 'find-function
  "hk" 'describe-key
  "hv" 'describe-variable
  "md" 'mark-defun
  "mf" #'kevin/make-frame
  "re" 'restart-emacs
  "tfw" 'toggle-full-window
  "tfm" 'toggle-frame-maximized
  "tff" 'toggle-frame-fullscreen
  )

;; window related keybindings
(evil-leader/set-key
  "0"  'select-window-0
  "1"  'select-window-1
  "2"  'select-window-2
  "3"  'select-window-3
  "4"  'select-window-4
  "7"  'select-window-7
  "8"  'select-window-8
  "9"  'select-window-9
  "wd" 'delete-window
  "w/" 'split-window-right
  "w-" 'split-window-below
  "wD" 'delete-other-windows
  )

;; buffer related keybindings
(evil-leader/set-key
  "bb" 'ivy-switch-buffer
  "bc" 'erase-message-buffer
  "be" 'eval-buffer
  "bd" 'kill-this-buffer
  "bf" 'beginning-of-defun
  "bi"  #'kevin/indent-region-or-buffer
  "bl" 'ibuffer-list-buffers
  "bm" #'kevin/kill-all-buffers
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bg" #'kevin/revert-buffer-no-confirm
  "bs" 'save-buffer
  "bS" #'kevin/create-scratch-buffer
  )

(provide 'init-keybinds)
;;; init-keybinds ends here
