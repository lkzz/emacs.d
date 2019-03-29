;;; init-windows.el --- window config for emacs. -*- lexical-binding: t; -*-
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

;; Interactively highlight the current-window (by dimming the others)
(use-package dimmer
  :hook (after-init . dimmer-mode)
  :config
  (setq dimmer-fraction 0.2))

;; Restore old window configurations
(use-package winner
  :hook (after-init . winner-mode)
  :init
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))

;; Quickly switch windows
(use-package ace-window
  :init
  (kevin/set-leader-keys "wo" #'ace-window))

;;;###autoload
(defun kevin/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;;;###autoload
(defun kevin/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;; Numbered window shortcuts
(use-package winum
  :hook (after-init . winum-mode)
  :init
  (setq window-numbering-scope 'global
        winum-auto-setup-mode-line nil
        winum-ignored-buffers '(" *which-key*")
        winum-auto-assign-0-to-minibuffer t)
  ;; window related keybindings
  (kevin/declare-prefix "w" "window")
  (kevin/set-leader-keys "0"  'winum-select-window-0-or-10
                         "1"  'winum-select-window-1
                         "2"  'winum-select-window-2
                         "3"  'winum-select-window-3
                         "4"  'winum-select-window-4
                         "wd" 'delete-window
                         "w/" #'kevin/split-window-right-and-focus
                         "w-" #'kevin/split-window-below-and-focus
                         "wD" 'delete-other-windows))

;; Zoom window like tmux
(use-package zoom-window
  :bind ("C-x C-z" . zoom-window-zoom)
  :init (setq zoom-window-mode-line-color "DarkGreen"))

(use-package centered-window
  :init
  (setq cwm-use-vertical-padding t
        cwm-frame-internal-border 15
        cwm-incremental-padding t
        cwm-left-fringe-ratio 0)
  (kevin/set-leader-keys "wc" #'centered-window-mode))

;; resize window
(use-package resize-window
  :init
  (kevin/set-leader-keys "wr" #'resize-window))

;;;###autoload
(defun kevin/toggle-golden-ratio ()
  "Golden ratio mode toggle function."
  (interactive)
  (if golden-ratio-mode
      (progn
        (golden-ratio-mode -1)
        (message "golden ratio disabled")
        (balance-windows))
    (progn
      (golden-ratio-mode 1)
      (message "golden ratio enabled")
      (golden-ratio))))

(use-package golden-ratio
  :diminish golden-ratio-mode "ⓖ"
  :init
  (kevin/set-leader-keys "tg" #'kevin/toggle-golden-ratio)
  :config
  ;; golden-ratio-exclude-modes
  (dolist (m '("bs-mode"
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
               "ranger-mode"
               ))
    (add-to-list 'golden-ratio-exclude-modes m))
  (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")
  ;; golden-ratio-extra-commands
  (dolist (f '(ace-window
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
    (add-to-list 'golden-ratio-extra-commands f))
  ;; golden-ratio-exclude-buffer-names
  (dolist (n '("*NeoTree*"
               "*LV*"
               "*which-key*"))
    (add-to-list 'golden-ratio-exclude-buffer-names n)))

(use-package centered-cursor-mode
  :commands (centered-cursor-mode global-centered-cursor-mode)
  :diminish centered-cursor-mode "⊝"
  :init
  (kevin/set-leader-keys "t-" 'centered-cursor-mode)
  (setq ccm-recenter-at-end-of-file t
        ccm-ignored-commands '(mouse-drag-region
                               mouse-set-point
                               widget-button-click
                               scroll-bar-toolkit-scroll
                               evil-mouse-drag-region))
  (global-centered-cursor-mode +1))

(provide 'init-windows)
;;; init-windows ends here
