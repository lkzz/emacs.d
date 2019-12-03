;;; init-keybinds.el --- personal keybinds,should be required finally. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(when kevin-mac-p
  (setq mac-command-modifier 'super) ; make Super key do command
  (setq mac-option-modifier 'meta)  ; make Option key do meta
  (setq mac-right-option-modifier 'none) ;; disable right Option key
  (setq mac-control-modifer 'control)
  (global-set-key [(super a)] 'mark-whole-buffer)
  (global-set-key [(super v)] 'yank)
  (global-set-key [(super c)] 'kill-ring-save)
  (global-set-key [(super s)] 'save-buffer)
  (global-set-key [(super w)]
                  (lambda () (interactive) (delete-window)))
  (global-set-key [(super z)] 'undo))

;; used as tmux prefix key
(global-unset-key (kbd "C-q"))
(define-key global-map (kbd "RET") 'newline-and-indent)

(kevin/declare-prefix "c" "comment")
(kevin/declare-prefix "f" "file")
(kevin/declare-prefix "l" "layout")
(kevin/declare-prefix "p" "project")
(kevin/declare-prefix "s" "search")

;; delete related keybindings
(kevin/declare-prefix "d" "delete")
(kevin/set-leader-keys
  "dd" #'kevin/delete-delimiter-enclosed-text
  "df" 'delete-frame
  "dw" 'kevin/delete-word)

;; Help related keybindings
(kevin/declare-prefix "h" "help")
(kevin/set-leader-keys
  "hd" 'describe-function
  "hf" 'find-function
  "hk" 'describe-key
  "hv" 'describe-variable)

(kevin/declare-prefix "j" "jump")
(kevin/set-leader-keys
  "jf" 'beginning-of-defun
  "jm" 'kevin/jump-match-delimiter)

;; Open applications with from emacs
(kevin/declare-prefix "o" "open")
(kevin/set-leader-keys
  "oi" #'kevin/open-init-file
  "ot" #'kevin/open-iterm
  "ow" #'kevin/open-wechat
  "oy" #'kevin/open-youdao)

(kevin/declare-prefix ";" "custom")
(kevin/set-leader-keys
  ";=" 'kevin/increase-fontsize
  ";-" 'kevin/decrease-fontsize)

;; Toggle
(kevin/declare-prefix "t" "toggle")
(kevin/set-leader-keys
  "tb" 'toggle-scroll-bar
  "tw" 'toggle-word-wrap
  "t;" 'toggle-frame-fullscreen
  "tt" 'toggle-truncate-lines
  "tg" 'kevin/toggle-golden-ratio
  "t'" 'kevin/toggle-aweshell)

;; window related keybindings
(kevin/declare-prefix "w" "window")
(kevin/set-leader-keys
  "1"  'winum-select-window-1
  "2"  'winum-select-window-2
  "3"  'winum-select-window-3
  "4"  'winum-select-window-4
  "5"  'winum-select-window-5
  "6"  'winum-select-window-6
  "7"  'winum-select-window-7
  "8"  'winum-select-window-8
  "9"  'winum-select-window-9
  "wd" 'delete-window
  "wo" 'other-window
  "wd" 'delete-window
  "w/" #'kevin/split-window-right-and-focus
  "w-" #'kevin/split-window-below-and-focus
  "wD" 'delete-other-windows)

(provide 'init-keybinds)
;;; init-keybinds ends here
