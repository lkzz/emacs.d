;; init-highlight.el --- Initialize highlight configurations. -*- lexical-binding: t; -*-
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

;; Highlight the current line
(use-package hl-line
  :straight (:type built-in)
  :hook (((evil-visual-state-entry activate-mark) . my/disable-hl-line)
         ((evil-visual-state-exit deactivate-mark) . my/enable-hl-line)
         ((prog-mode text-mode conf-mode special-mode) . hl-line-mode))
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)
  ;; Temporarily disable `hl-line' when selection is active
  (defvar my/-hl-line-mode nil)
  (defun my/disable-hl-line ()
    (when hl-line-mode
      (hl-line-mode -1)
      (setq-local my/-hl-line-mode t)))
  (defun my/enable-hl-line ()
    (when my/-hl-line-mode
      (hl-line-mode +1))))

;; Show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(use-package paren
  :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (set-face-foreground 'show-paren-match "red")      ;定义前景色
  (set-face-bold 'show-paren-match t)                ;加粗显示括号匹配
  (set-face-background 'show-paren-match nil)        ;定义背景色
  (set-face-underline 'show-paren-match t)           ;显示下划线
  (setq show-paren-delay 0.1
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Highlight indent guide.
(use-package highlight-indent-guides
  :if (display-graphic-p)
  :diminish highlight-indent-guides-mode
  :hook ((prog-mode text-mode conf-mode protobuf-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-delay 0.3
        highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled nil
        highlight-indent-guides-suppress-auto-error t)
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  ;; Don't display first level of indentation
  (defun my/indent-guides-for-all-but-first-column (level responsive display)
    (unless (< level 1)
      (highlight-indent-guides--highlighter-default level responsive display)))
  (setq highlight-indent-guides-highlighter-function #'my/indent-guides-for-all-but-first-column))

;; Highlight number.
(use-package highlight-numbers
  :if (display-graphic-p)
  :hook ((prog-mode conf-mode protobuf-mode yaml-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((emacs-lisp-mode conf-space-mode) . rainbow-mode))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 3))

;; Highlight TODO/FIXME/NOTE...
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces `(("BUG" error bold)
                                ("FIXME" error bold)
                                ("TODO" warning bold)
                                ("NOTE" success bold)
                                ("HACK" font-lock-constant-face bold)
                                ("REVIEW" font-lock-keyword-face bold)
                                ("DEPRECATED" font-lock-doc-face bold))))

;; Pulse current line
(use-package pulse
  :straight (:type built-in)
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
  :hook (((dumb-jump-after-jump imenu-after-jump) . my/recenter-and-pulse)
         ((bookmark-after-jump magit-diff-visit-file next-error) . my/recenter-and-pulse-line))
  :init
  (defun my/pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun my/pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (my/pulse-momentary-line)))

  (defun my/recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (my/pulse-momentary))

  (defun my/recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my/pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window windmove-do-window-select
                 ace-window
                 winum-select-window-1
                 winum-select-window-2
                 winum-select-window-3
                 winum-select-window-4
                 winum-select-window-5
                 winum-select-window-6
                 winum-select-window-7
                 winum-select-window-8
                 winum-select-window-9
                 winum-select-window-0-or-10
                 pager-page-down pager-page-up
                 lsp-bridge-ref-jump-next-keyword lsp-bridge-ref-jump-prev-keyword
                 symbol-overlay-basic-jump))
    (advice-add cmd :after #'my/pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 neotree-enter
                 xref-go-back xref-go-forward
                 evil-jump-backward evil-jump-forward
                 better-jumper-jump-forward better-jumper-jump-backward
                 diff-hl-previous-hunk diff-hl-next-hunk
                 goto-last-change))
    (advice-add cmd :after #'my/recenter-and-pulse)))

;; Highlight symbols, copy from centaur emacs
(use-package symbol-overlay
  :diminish
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-r" . symbol-overlay-rename)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-C" . symbol-overlay-remove-all))
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
  (with-eval-after-load 'all-the-icons
    (setq symbol-overlay-faces
          '((:inherit (all-the-icons-blue bold) :inverse-video t)
            (:inherit (all-the-icons-pink bold) :inverse-video t)
            (:inherit (all-the-icons-yellow bold) :inverse-video t)
            (:inherit (all-the-icons-purple bold) :inverse-video t)
            (:inherit (all-the-icons-red bold) :inverse-video t)
            (:inherit (all-the-icons-orange bold) :inverse-video t)
            (:inherit (all-the-icons-green bold) :inverse-video t)
            (:inherit (all-the-icons-cyan bold) :inverse-video t))))
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode 'yaml-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))

(provide 'init-highlight)
;;; init-highlight.el ends here
