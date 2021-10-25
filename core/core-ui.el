;;; core-ui.el --- 优化Emacs默认UI -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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

(defvar kevin-load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme'.")

(defun kevin/run-load-theme-hooks (&rest _)
  (run-hooks 'kevin-load-theme-hook))

(advice-add #'load-theme :after #'kevin/run-load-theme-hooks)

(setq inhibit-startup-screen t            ; 禁止启动画面
      inhibit-startup-echo-area-message t ; 禁止echo area message
      inhibit-default-init t              ; 禁止加载default lib
      mouse-yank-at-point t
      initial-buffer-choice  nil
      initial-scratch-message (format ";; Happy Hacking, %s - Emacs ♥ You!\n" user-full-name)
      initial-major-mode 'fundamental-mode) ; 设置默认的major mode
(fset #'display-startup-echo-area-message #'ignore)

(when is-mac-p
  ;; sane trackpad/mouse scroll settings
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))

(setq scroll-step 1
      scroll-margin 0
      hscroll-step 1
      hscroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      auto-window-vscroll nil
      fast-but-imprecise-scrolling nil
      scroll-preserve-screen-position t)

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(add-hook 'eshell-mode-hook (lambda() (setq hscroll-margin 0)))
(add-hook 'term-mode-hook (lambda() (setq hscroll-margin 0)))

;; Enable mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(blink-cursor-mode -1)                  ; 禁止光标闪烁
(setq ring-bell-function 'ignore)       ; 关闭警告提示音

(use-package simple
  :straight (:type built-in)
  :hook ((after-init . size-indication-mode) ; 显示百分比进度
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq line-number-mode t              ; 打开行号显示
        column-number-mode t            ; 打开列号显示
        kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil            ; Move line by visual line
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        column-number-indicator-zero-based nil ; column starts from 1
        kill-do-not-save-duplicates t ; eliminate duplicates
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again
  ;; 设置visual line fringe bitmap
  (when (and (fboundp 'define-fringe-bitmap) (display-graphic-p))
    (define-fringe-bitmap 'right-curly-arrow
      [#b00000000
       #b01111100
       #b01111100
       #b00001100
       #b00001100
       #b00000000
       #b00000000])
    (define-fringe-bitmap 'left-curly-arrow
      [#b00000000
       #b00110000
       #b00110000
       #b00111110
       #b00111110
       #b00000000
       #b00000000])
    (set-fringe-bitmap-face 'right-curly-arrow 'warning)
    (set-fringe-bitmap-face 'left-curly-arrow 'warning)
    (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(use-package so-long
  :straight (:type built-in)
  :hook (after-init . global-so-long-mode)
  :config
  (setq so-long-threshold 400)
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; Text files could possibly be too long too
  (add-to-list 'so-long-target-modes 'text-mode)
  ;; disable some mode that may be unnecessary/expensive for large buffer
  (add-to-list 'so-long-minor-modes 'rainbow-mode)
  (add-to-list 'so-long-minor-modes 'flycheck-mode)
  (add-to-list 'so-long-minor-modes 'eldoc-mode)
  (add-to-list 'so-long-minor-modes 'ws-butler-mode)
  (add-to-list 'so-long-minor-modes 'undo-tree-mode)
  (add-to-list 'so-long-minor-modes 'highlight-numbers-mode)
  (add-to-list 'so-long-minor-modes 'rainbow-delimiters-mode)
  (add-to-list 'so-long-minor-modes 'highlight-indent-guides-mode))

;; Indentation
(setq-default tab-width 4
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 100)

;; Word wrapping
(setq-default word-wrap nil
              truncate-lines nil
              show-trailing-whitespace nil ; Don't show trailing whitespace by default
              truncate-partial-width-windows nil)

;; Enable truncate lines in minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda () (setq truncate-lines t)))

;;============================ fringe start ==========================================
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

;; doesn't exist in terminal Emacs; we define it to prevent errors
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))
;;============================ fringe end ==========================================

;;============================ window start ========================================
;; 移除工具栏
(tool-bar-mode -1)
;; 移除菜单栏
(menu-bar-mode -1)
;; 移除滚动条
(scroll-bar-mode -1)
;;; 禁止使用对话框
(setq use-file-dialog nil
      use-dialog-box nil)

;; 标题栏格式设置
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(when is-mac-p
  ;; NOTE Meaningless to railwaycat's emacs-mac build
  (setq ns-use-native-fullscreen nil)
  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; 打开文件时不再创建新的frame
  (setq ns-pop-up-frames nil))

;; Don't resize emacs in steps, it looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'kevin-load-theme-hook #'window-divider-mode)
;;============================ window end ==========================================

;;============================ minibuffer start ====================================

;; Completion engine
(use-package minibuffer
  :straight (:type built-in)
  :bind (:map minibuffer-local-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-ns-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-completion-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-must-match-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-isearch-map
         ([escape] . abort-recursive-edit))
  :custom
  (completion-auto-help t)
  (completion-show-help nil)
  ;; Cycle completions regardless of the count
  (completion-cycle-threshold t)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-eldef-shorten-default t)
  (minibuffer-electric-default-mode t)
  ;; One frame one minibuffer.
  (minibuffer-follows-selected-frame nil)
  ;; Ignore cases when complete
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; `selectrum', `vertico' and `icomplete' will honoring
  (completion-styles '(basic partial-completion substring flex))
  (completion-category-overrides '((buffer (styles . (flex)))))
  ;; vertical view
  (completions-format 'one-column)
  (completions-detailed t))

(setq echo-keystrokes 0.02
      enable-recursive-minibuffers t
      resize-mini-windows 'grow-only
      max-mini-window-height 0.15)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;;============================ minibuffer end ======================================

;; Improve display
(setq display-raw-bytes-as-hex t)

;; Treats the `_' as a word constituent
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(provide 'core-ui)
;;; core-ui.el ends here
