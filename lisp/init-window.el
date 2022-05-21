;;; init-window.el --- window config for emacs. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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
  :bind ([remap other-window] . ace-window)
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
  :init
  (setq window-numbering-scope 'global
        winum-auto-setup-mode-line nil
        winum-ignored-buffers '(" *which-key*")
        winum-auto-assign-0-to-minibuffer t))

;; Zoom window like tmux
(use-package zoom-window
  :init (setq zoom-window-mode-line-color "DarkGreen"))

(use-package golden-ratio
  :diminish golden-ratio-mode "ⓖ"
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

;; popup rules from centaur emacs
(use-package popper
  :defines popper-echo-dispatch-actions
  :commands popper-group-by-projectile
  :bind (:map popper-mode-map
         ("C-h z" . popper-toggle-latest)
         ("C-<tab>"   . popper-cycle)
         ("C-M-<tab>" . popper-toggle-type))
  :hook (after-init . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
          "\\*Finder\\*"
          "\\*Embark Actions\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          ivy-occur-mode ivy-occur-grep-mode
          process-menu-mode list-environment-mode cargo-process-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode
          lsp-bridge-ref-mode

          "^\\*eshell.*\\*$" eshell-mode
          "^\\*shell.*\\*$"  shell-mode
          "^\\*term.*\\*$"   term-mode
          "^\\*vterm.*\\*$"  vterm-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-test-moed))

  (with-eval-after-load 'projectile
    (setq popper-group-function #'popper-group-by-projectile))

  (when (display-grayscale-p)
    (setq popper-mode-line
          '(:eval
            (format " %s " (all-the-icons-octicon "pin" :height 0.9 :v-adjust 0.0 :face 'mode-line-emphasis)))))

  (setq popper-echo-dispatch-actions t)
  :config
  (popper-echo-mode 1)

  (defun my-popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 3)
     (floor (frame-height) 3)))
  (setq popper-window-height #'my-popper-fit-window-height)

  (defun popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (advice-add #'keyboard-quit :before #'popper-close-window-hack))

(provide 'init-window)
;;; init-window ends here
