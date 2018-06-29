;;; init-keybinds.el --- personal keybinds,should be required finally.
;;; Commentary:
;;; Code:

(kevin/declare-prefix "c" "comment")
(kevin/declare-prefix "f" "file")
(kevin/set-leader-keys "fi" #'kevin/open-init-file)
(kevin/declare-prefix "h" "help")
(kevin/declare-prefix "j" "jump")
(kevin/declare-prefix "o" "personal")
(kevin/declare-prefix "p" "projectile")

;; bookmark
(kevin/declare-prefix "ob" "bookmark")
(kevin/set-leader-keys
  "obs" 'bookmark-set
  "obr" 'bookmark-rename
  "obd" 'bookmark-delete
  "obj" 'counsel-bookmark
  "obl" 'bookmark-bmenu-list)

;; toggle
(kevin/declare-prefix "t" "toggle")
(kevin/set-leader-keys
  "tM" 'toggle-major-mode
  "tb" 'toggle-scroll-bar
  "ti" 'imenu-list-smart-toggle
  "tw" 'toggle-word-wrap
  "tm" 'toggle-frame-maximized
  "tf" 'toggle-frame-fullscreen)

;; open applications with from emacs
(kevin/declare-prefix "a" "application")
(kevin/set-leader-keys
  "aa" 'counsel-osx-app
  "ai" #'kevin/open-iterm ;; open item2
  "aw" #'kevin/open-wechat ; open WeChat
  "ay" #'kevin/open-youdao ; open youdao dictionary
  )

;; misc related keybindings
(kevin/set-leader-keys
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
