;;; core-ui.el --- 优化Emacs默认UI -*- lexical-binding: t -*-
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

;; Personal information
(setq user-full-name "lkzz"
      user-mail-address "kevin.scnu@gmail.com")

;; 标题栏格式设置
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

;; Transparent title bar
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; 简化yes-or-no 输入
(setq use-short-answers t)

;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
(setq y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

;; Cutting and pasting use primary/clipboard
(setq select-enable-primary t
      select-enable-clipboard t)

;; No backup files
(setq make-backup-files nil
      auto-save-default nil)

;; No Lock files
(setq create-lockfiles nil)

;; Move to trash when delete file
(setq delete-by-moving-to-trash t)

;; No annoying bell
(setq ring-bell-function 'ignore)

;; No eyes distraction
(setq blink-cursor-mode nil)

;; Remove tool bar
(tool-bar-mode -1)

;; Remove menu bar
(menu-bar-mode -1)

;; Remove scroll bar
(scroll-bar-mode -1)

(setq mouse-yank-at-point t)

;; Disable dialog
(setq use-dialog-box nil
      use-file-dialog nil)

;; Suppress GUI features and more
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message t
      inhibit-default-init t
      inhibit-x-resources t)

;; Some initital option
(setq initial-buffer-choice nil
      initial-scratch-message (format ";; Happy Hacking, %s - Emacs ♥ You!\n" user-full-name)
      initial-major-mode 'fundamental-mode) ; 设置默认的major mode

(fset #'display-startup-echo-area-message #'ignore)


;; Misc
(setq confirm-nonexistent-file-or-buffer t ; Whether confirmation is requested before visiting a new file or buffer.
      confirm-kill-processes nil           ; kill running processes without confirmation on Emacs exit
      inhibit-compacting-font-caches t     ; gc 忽略字体缓存
      find-file-visit-truename t           ; 当是链接时，显示真正的连接
      uniquify-buffer-name-style 'forward)

;; Emacs "updates" its ui more often than it needs to, so we slow it down slightly.
(setq idle-update-delay 1)

;; Optimize for very long lines
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;; Disable cursor in non-selected windows.
(setq-default cursor-in-non-selected-windows nil)

;; Non-nil means highlight region even in nonselected windows.
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; Improve display
(setq display-raw-bytes-as-hex t)

;; Make scrolling smoother by avoiding unnecessary fontification
(setq redisplay-skip-fontification-on-input t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless (eq system-type 'darwin) (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux) (setq command-line-x-option-alist nil))

(when is-mac-p
  ;; sane trackpad/mouse scroll settings
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))

;; Scroll
(setq scroll-step 1
      scroll-margin 0
      hscroll-step 1
      hscroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(add-hook 'eshell-mode-hook (lambda() (setq hscroll-margin 0)))
(add-hook 'term-mode-hook (lambda() (setq hscroll-margin 0)))

;; Enable mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

;; Indentation
(setq-default tab-width 4
              fill-column 80
              tab-always-indent t
              indent-tabs-mode nil)

;; Word wrapping
(setq-default word-wrap nil
              truncate-lines t
              truncate-partial-width-windows nil)

;; Whitespace trailing
(setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
(defun my/enable-trailing-whitespace ()
  "Show trailing spaces and delete on saving."
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

;; Only enable whitespace trailing in specify mode
(add-hook 'prog-mode-hook #'my/enable-trailing-whitespace)
(add-hook 'markdown-mode-hook #'my/enable-trailing-whitespace)
(add-hook 'conf-mode-hook #'my/enable-trailing-whitespace)

;; Fringe config
(when (fboundp 'set-fringe-mode)
  (set-fringe-mode '(4 . 8)))
(setq-default fringes-outside-margins nil
              indicate-buffer-boundaries nil    ; 不显示buffer界限fringe
              fringe-indicator-alist (assq-delete-all
                                      'truncation
                                      (assq-delete-all
                                       'continuation
                                       fringe-indicator-alist))
              indicate-empty-lines nil)         ; 不显示空行fringe

;; Doesn't exist in terminal Emacs; we define it to prevent errors
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))

(when is-mac-p
  ;; NOTE Meaningless to railwaycat's emacs-mac build
  (setq ns-use-native-fullscreen nil)
  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; 打开文件时不再创建新的frame
  (setq ns-pop-up-frames nil))

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

(setq echo-keystrokes 0.02
      resize-mini-windows 'grow-only
      max-mini-window-height 0.15)

;; Treats the `_' as a word constituent
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

;; Suppressing ad-handle-definition warnings.
(setq ad-redefinition-action 'accept)

(provide 'core-basic)
;;; core-basic.el ends here
