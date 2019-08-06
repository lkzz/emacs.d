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

(kevin/declare-prefix "c" "comment")
(kevin/declare-prefix "f" "file")
(kevin/declare-prefix "h" "help")
(kevin/declare-prefix "j" "jump")
(kevin/declare-prefix "p" "project")
(kevin/declare-prefix "q" "quit")
(kevin/declare-prefix "s" "search")

(kevin/set-leader-keys "fi" #'kevin/open-init-file)

;; Toggle
(kevin/declare-prefix "t" "toggle")
(kevin/set-leader-keys
  "tM" 'toggle-major-mode
  "tb" 'toggle-scroll-bar
  "tw" 'toggle-word-wrap
  ;; "tm" 'toggle-frame-maximized
  "tf" 'toggle-frame-fullscreen
  "tp" 'persp-mkevin/set-leader-keys
  "tt" 'toggle-truncate-lines)

;; Open applications with from emacs
(kevin/declare-prefix "a" "application")
(kevin/set-leader-keys
  "ai" #'kevin/open-iterm
  "aw" #'kevin/open-wechat
  "ay" #'kevin/open-youdao)

;; Misc related keybindings
(kevin/set-leader-keys
  "=" 'kevin/increase-fontsize
  "-" 'kevin/decrease-fontsize
  "hd" 'describe-function
  "hf" 'find-function
  "hk" 'describe-key
  "hv" 'describe-variable
  "qq" 'save-buffers-kill-terminal)

;; Frame releated keybindings
(kevin/declare-prefix "F" "frame")
(kevin/set-leader-keys
  "Fm" #'kevin/make-frame
  "Fd" 'delete-frame)

;; Delimiter releated keybindings
(kevin/declare-prefix "d" "delimiter")
(kevin/set-leader-keys
  "dj" #'kevin/goto-match-delimiter
  "dd" #'kevin/delete-delimiter-enclosed-text)

;; used as tmux prefix key
(global-unset-key (kbd "C-q"))
(define-key global-map (kbd "RET") 'newline-and-indent)

(provide 'init-keybinds)
;;; init-keybinds ends here
