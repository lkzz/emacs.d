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
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta))

;;
;;; Universal, non-nuclear escape
;; refer page:https://github.com/hlissner/doom-emacs/blob/develop/core/core-keybinds.el
;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar kevin-keyboard-quit-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).")

(defun kevin/keyboard-quit ()
  "Run `kevin-keyboard-quit-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'kevin-keyboard-quit-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'kevin/keyboard-quit)

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
