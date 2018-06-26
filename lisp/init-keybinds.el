;;; init-keybinds.el --- personal keybinds,should be required finally.
;;; Commentary:
;;; Code:

(kevin/declare-prefix "c" "comment")
(kevin/declare-prefix "f" "file")
(kevin/declare-prefix "g" "magit")
(kevin/declare-prefix "h" "help")
(kevin/declare-prefix "j" "jump")
(kevin/declare-prefix "l" "layout")
(kevin/declare-prefix "o" "personal")
(kevin/declare-prefix "p" "projectile")

;; bookmark
(kevin/declare-prefix "ob" "bookmark")
(evil-leader/set-key
  "obs" 'bookmark-set
  "obr" 'bookmark-rename
  "obd" 'bookmark-delete
  "obj" 'counsel-bookmark
  "obl" 'bookmark-bmenu-list)

;; toggle
(kevin/declare-prefix "t" "toggle")
(evil-leader/set-key
  "tM" 'toggle-major-mode
  "tb" 'toggle-scroll-bar
  "ti" 'imenu-list-smart-toggle
  "tw" 'toggle-word-wrap
  "tm" 'toggle-frame-maximized
  "tn" 'nlinum-mode
  "tf" 'toggle-frame-fullscreen
  "tg" 'golden-ratio-mode)

;; open applications with from emacs
(kevin/declare-prefix "a" "application")
(evil-leader/set-key
  "aa" 'counsel-osx-app
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
  )

;; window related keybindings
(kevin/declare-prefix "w" "window")
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
(kevin/declare-prefix "b" "buffer")
(evil-leader/set-key
  "bb" 'ivy-switch-buffer
  "bc" 'cleanup-buffer
  "be" 'eval-buffer
  "bd" 'kill-this-buffer
  "bD" #'kevin/kill-other-buffers
  "bf" 'beginning-of-defun
  "bi"  #'kevin/indent-region-or-buffer
  "bk" 'kill-buffer
  "bl" 'ibuffer-list-buffers
  "bm" #'kevin/kill-all-buffers
  "bp" #'kevin/switch-to-prev-buffer
  "bn" #'kevin/switch-to-next-buffer
  "bg" #'kevin/revert-buffer-no-confirm
  "bs" 'save-buffer
  "bS" #'kevin/create-scratch-buffer
  )


;; ;; * Global Keybindings
;; ;; `general-define-key' acts like `global-set-key' when :keymaps is not
;; ;; specified (because ":keymaps 'global" is the default)
;; ;; kbd is not necessary and arbitrary amount of key def pairs are allowed
;; (general-define-key
;;  "M-x" 'smex                             ; or 'smex
;;  "C-s" 'counsel-grep-or-swiper)

(bind-map kevin/default-map
  :keys (kevin/emacs-leader-key)
  :evil-keys (kevin/leader-key))

(provide 'init-keybinds)
;;; init-keybinds ends here
