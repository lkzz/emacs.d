;;; init-windows.el --- window config for emacs. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

;; Interactively highlight the current-window (by dimming the others)
(use-package dimmer
  :defer t
  :init (add-hook 'after-init-hook #'dimmer-mode)
  :config
  (setq dimmer-fraction 0.2))

;; Restore old window configurations
(use-package winner
  :defer t
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
                                "*esh command on file*"))
  (add-hook 'after-init-hook #'winner-mode))

;; Quickly switch windows
(use-package ace-window
  :defer t
  :ensure t
  :init
  (progn
    (kevin/set-leader-keys "wo" #'ace-window)))

;;;###autoload
(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))


;;;###autoload
(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;; Numbered window shortcuts
(use-package window-numbering
  :defer t
  :ensure t
  :hook ((after-init . window-numbering-mode)
         ;; don't add numbers to the modeline
         (window-numbering-mode . window-numbering-clear-mode-line))
  :init
  (progn
    (kevin/declare-prefix "w" "window")
    ;; window related keybindings
    (kevin/set-leader-keys
      "1"  'select-window-1
      "2"  'select-window-2
      "3"  'select-window-3
      "4"  'select-window-4
      "wd" 'delete-window
      "w/" #'split-window-right-and-focus
      "w-" #'split-window-below-and-focus
      "wD" 'delete-other-windows)))

;; Zoom window like tmux
(use-package zoom-window
  :defer t
  :ensure t
  :bind ("C-x C-z" . zoom-window-zoom)
  :init (setq zoom-window-mode-line-color "DarkGreen"))

(use-package centered-window
  :defer t
  :ensure t
  :init
  (progn
    (setq cwm-use-vertical-padding t)
    (setq cwm-frame-internal-border 15)
    (setq cwm-incremental-padding t)
    (setq cwm-left-fringe-ratio 0)
    (kevin/set-leader-keys "wc" #'centered-window-mode)))

;; Popup Window Manager
(use-package popwin
  :defer t
  :ensure t
  :commands popwin-mode
  :init (add-hook 'after-init-hook #'popwin-mode)
  :config
  (bind-key "C-z" popwin:keymap)
  ;; don't use default value but manage it ourselves
  (setq popwin:special-display-config
        '(;; Emacs
          ("*Help*" :dedicated t :position bottom :stick nil :noselect nil)
          ("*compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Compile-Log*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Warnings*" :dedicated t :position bottom :stick t :noselect t)
          ("*Completions*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*Async Shell Command\*.+" :regexp t :position bottom :stick t :noselect nil)
          ("^*Man.+*$" :regexp t :position bottom :stick nil :noselect nil :height 0.4)
          ("^*WoMan.+*$" :regexp t :position bottom)
          ("^*Backtrace.+*$" :regexp t :dedicated t :position bottom :stick t :noselect nil)
          ;; Kill Ring
          ("*Kill Ring*" :dedicated t :position bottom)
          ;; Flycheck
          ("\*flycheck errors\*.+*$" :regexp t :position bottom :stick t :noselect nil)
          ;; Youdao dict
          ("*Youdao Dictionary*" :dedicated t :position bottom)
          ;; Paradox
          ("*Paradox Report*" :dedicated t :position bottom :noselect nil)
          ;; List
          ("*Colors*" :dedicated t :position bottom)
          ("*Process List*" :dedicated t :position bottom)
          ("*Process-Environment*" :dedicated t :position bottom)
          ;; undo-tree
          (" *undo-tree*" :dedicated t :position right :stick t :noselect nil :width 60)
          ;; Search
          ("*grep*" :dedicated t :position bottom :stick t :noselect nil)
          ("*ag search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*rg*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*pt-search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*Occur*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*ivy-occur.+*$" :regexp t :position bottom :stick t :noselect nil)
          ("*xref*" :dedicated t :position bottom :stick nil :noselect nil)
          ;; VC
          ("*vc-diff*" :dedicated t :position bottom :stick t :noselect nil)
          ("*vc-change-log*" :dedicated t :position bottom :stick t :noselect nil)
          ;; Magit
          (magit-status-mode :dedicated t :position bottom :stick t :height 0.5)
          (magit-diff-mode :dedicated t :position bottom :stick t :noselect t :height 0.5)
          ;; Script
          ("*shell*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Python*" :dedicated t :position bottom :stick t :noselect t)
          ("*Ruby*" :dedicated t :position bottom :stick t :noselect t)
          ("*quickrun*" :dedicated t :position bottom :stick t :noselect t)
          ;; Go
          ("^*godoc.+*$" :regexp t :position bottom :stick nil :noselect nil)
          ("*golint*" :dedicated t :position bottom :stick t :noselect nil)
          ("*govet*" :dedicated t :position bottom :stick t :noselect nil)
          ("*go-guru-output*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Gofmt Errors*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Go Test*" :dedicated t :position bottom :stick t :noselect nil)
          ;; Test
          ("*ert*" :dedicated t :position bottom :stick t :noselect nil)
          ("*nosetests*" :dedicated t :position bottom :stick t :noselect nil))))

;; ;; Easy window config switching
;; (use-package eyebrowse
;;   :defer t
;;   :ensure t
;;   :init (add-hook 'after-init-hook #'eyebrowse-mode))

;; resize window
(use-package resize-window
  :defer t
  :ensure t
  :init
  (progn
    (kevin/set-leader-keys "wr" #'resize-window)))


(use-package golden-ratio
  :defer t
  :ensure t
  :diminish golden-ratio-mode "ⓖ"
  :init
  (progn
    (defun kevin/golden-ratio-toggle ()
      (interactive)
      (if golden-ratio-mode
          (progn
            (golden-ratio-mode -1)
            (message "golden ratio disabled")
            (balance-windows))
        (progn
          (golden-ratio-mode 1)
          (message "golden ratio enabled"))))
    (kevin/set-leader-keys "tg" #'kevin/golden-ratio-toggle))
  :config
  (progn
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
    (dolist (n '(" *NeoTree*"
                 "*LV*"
                 " *which-key*"))
      (add-to-list 'golden-ratio-exclude-buffer-names n))
    ))

(use-package centered-cursor-mode
  :defer t
  :ensure t
  :commands (centered-cursor-mode
             global-centered-cursor-mode)
  :diminish centered-cursor-mode "⊝"
  :init
  (progn
    (kevin/set-leader-keys "t-" 'centered-cursor-mode))
  (progn
    (setq ccm-recenter-at-end-of-file t
          ccm-ignored-commands '(mouse-drag-region
                                 mouse-set-point
                                 widget-button-click
                                 scroll-bar-toolkit-scroll
                                 evil-mouse-drag-region))
    (global-centered-cursor-mode +1)))

(provide 'init-windows)
;;; init-windows ends here
