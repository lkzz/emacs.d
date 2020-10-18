;;; init-window.el --- window config for emacs. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2020  Kevin Leung
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

;; Quickly switch windows
(use-package ace-window
  :commands ace-window
  :init
  (global-set-key [remap other-window] #'ace-window)
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

;; Numbered window shortcuts
(use-package winum
  :hook (after-init . winum-mode)
  :general
  (kevin/space-key-define
    "1"  'winum-select-window-1
    "2"  'winum-select-window-2
    "3"  'winum-select-window-3
    "4"  'winum-select-window-4
    "5"  'winum-select-window-5
    "6"  'winum-select-window-6
    "7"  'winum-select-window-7
    "8"  'winum-select-window-8
    "9"  'winum-select-window-9
    "w" '(nil :which-key "Window")
    "w c" 'centered-window-mode
    "w d" 'delete-window
    "w o" 'other-window
    "w d" 'delete-window
    "w z" 'zoom-window-zoom
    "w /" '(kevin/split-window-right-and-focus :wk "split-window-right")
    "w -" '(kevin/split-window-below-and-focus :wk "split-window-below")
    "w D" 'delete-other-windows)
  :init
  (setq window-numbering-scope 'global
        winum-auto-setup-mode-line nil
        winum-ignored-buffers '(" *which-key*")
        winum-auto-assign-0-to-minibuffer t))

;; Zoom window like tmux
(use-package zoom-window
  :init (setq zoom-window-mode-line-color "DarkGreen"))

(use-package centered-window
  :commands centered-window-mode
  :config
  (setq cwm-use-vertical-padding t
        cwm-frame-internal-border 15
        cwm-incremental-padding t
        cwm-left-fringe-ratio 0))

(use-package golden-ratio
  :diminish golden-ratio-mode "ⓖ"
  :general
  (kevin/space-key-define "t g" '(kevin/toggle-golden-ratio :wk "golden-ratio"))
  :config
  ;; golden-ratio-exclude-modes
  (dolist (mode '("bs-mode"
                  "calc-mode"
                  "ediff-mode"
                  "dired-mode"
                  "gud-mode"
                  "gdb-locals-mode"
                  "gdb-registers-mode"
                  "gdb-breakpoints-mode"
                  "gdb-threads-mode"
                  "gdb-frames-mode"
                  "gdb-inferior-io-mode"
                  "gdb-disassembly-mode"
                  "gdb-memory-mode"
                  "speedbar-mode"
                  "ranger-mode"))
    (add-to-list 'golden-ratio-exclude-modes mode))
  (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")
  ;; golden-ratio-extra-commands
  (dolist (cmd '(ace-window
                 ace-delete-window
                 ace-select-window
                 ace-swap-window
                 ace-maximize-window
                 avy-pop-mark
                 buf-move-left
                 buf-move-right
                 buf-move-up
                 buf-move-down
                 evil-avy-goto-word-or-subword-1
                 evil-avy-goto-line
                 evil-window-delete
                 evil-window-split
                 evil-window-vsplit
                 evil-window-left
                 evil-window-right
                 evil-window-up
                 evil-window-down
                 evil-window-bottom-right
                 evil-window-top-left
                 evil-window-mru
                 evil-window-next
                 evil-window-prev
                 evil-window-new
                 evil-window-vnew
                 evil-window-rotate-upwards
                 evil-window-rotate-downwards
                 evil-window-move-very-top
                 evil-window-move-far-left
                 evil-window-move-far-right
                 evil-window-move-very-bottom
                 quit-window
                 winum-select-window-0-or-10
                 winum-select-window-1
                 winum-select-window-2
                 winum-select-window-3
                 winum-select-window-4
                 winum-select-window-5
                 winum-select-window-6
                 winum-select-window-7
                 winum-select-window-8
                 winum-select-window-9
                 windmove-left
                 windmove-right
                 windmove-up
                 windmove-down))
    (add-to-list 'golden-ratio-extra-commands cmd))
  ;; golden-ratio-exclude-buffer-names
  (dolist (name '("*NeoTree*"
                  "*LV*"
                  "*which-key*"))
    (add-to-list 'golden-ratio-exclude-buffer-names name)))

(provide 'init-window)
;;; init-window ends here
