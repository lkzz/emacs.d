;;; init-ui.el ---  setup ui. -*- lexical-binding: t; -*-
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

(when is-mac-p
  ;; 打开抗锯齿
  (setq mac-allow-anti-aliasing t)
  ;; (setq ns-use-native-fullscreen nil)
  ;; natural title bar
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setcdr (assq 'ns-appearance default-frame-alist)
                      (frame-parameter nil 'background-mode)))))
;; 关闭srgb，修复modeline上的颜色显示问题
(setq ns-use-srgb-colorspace nil)
;; 去除全屏时的黑边
(setq frame-resize-pixelwise t)
;; 移除工具栏
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
;; 移除滚动条
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))
;; 移除菜单栏
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
;; 禁止启动画面
(setq inhibit-startup-screen t
      initial-buffer-choice  nil)
;; 设置scratch buffer message
(setq initial-scratch-message kevin-scratch-message)
;; 禁止使用对话框
(setq use-file-dialog nil
      use-dialog-box nil)
(setq inhibit-startup-echo-area-message t ; 禁止echo area message
      inhibit-default-init t              ; 禁止加载default lib
      initial-major-mode 'fundamental-mode) ; 设置默认的major mode
(fset #'display-startup-echo-area-message #'ignore)

;;=================== 鼠标设置 =======================================
;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Enable mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

;; 鼠标滚动设置
(when is-mac-p
  ;; sane trackpad/mouse scroll settings
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(add-hook 'eshell-mode-hook (lambda() (setq hscroll-margin 0)))
(add-hook 'term-mode-hook (lambda() (setq hscroll-margin 0)))
;;=================== 鼠标设置 =======================================

;;=================== 光标设置 =======================================
;; 禁止光标闪烁
(blink-cursor-mode -1)
;; Don't blink the paren matching the one at point.
(setq blink-matching-paren nil)
;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)
;;=================== 光标设置 =======================================

;; 禁止显示警告提示
(setq visible-bell nil)
;; 关闭警告提示音
(setq ring-bell-function 'ignore)

;; 标题栏格式设置
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))
;; 打开文件时不再创建新的frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't resize emacs in steps, it looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;;========================= 安装常用的主题 ===========================
(use-package doom-themes
  :defer t
  :config
  (doom-themes-org-config)
  (doom-themes-neotree-config))
;; 加载主题
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (load-theme kevin-theme-selected t)))
  (load-theme kevin-theme-selected t))

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)
;;========================= 安装常用的主题 ===========================

;; ;; 启动时全屏
;; (when (featurep 'cocoa)
;;   ;; 在Mac平台, Emacs不能进入Mac原生的全屏模式,否则会导致 `make-frame' 创建时也集成原生全屏属性后造成白屏和左右滑动现象.
;;   ;; 所以先设置 `ns-use-native-fullscreen' 和 `ns-use-fullscreen-animation' 禁止Emacs使用Mac原生的全屏模式.
;;   ;; 而是采用传统的全屏模式, 传统的全屏模式, 只会在当前工作区全屏,而不是切换到Mac那种单独的全屏工作区,
;;   ;; 这样执行 `make-frame' 先关代码或插件时,就不会因为Mac单独工作区左右滑动产生的bug.
;;   ;; Mac平台下,不能直接使用 `set-frame-parameter' 和 `fullboth' 来设置全屏,
;;   ;; 那样也会导致Mac窗口管理器直接把Emacs窗口扔到单独的工作区, 从而对 `make-frame' 产生同样的Bug.
;;   ;; 所以, 启动的时候通过 `set-frame-parameter' 和 `maximized' 先设置Emacs为最大化窗口状态, 启动5秒以后再设置成全屏状态,
;;   ;; Mac就不会移动Emacs窗口到单独的工作区, 最终解决Mac平台下原生全屏窗口导致 `make-frame' 左右滑动闪烁的问题.
;;   (setq ns-use-native-fullscreen nil)
;;   (setq ns-use-fullscreen-animation nil)
;;   (run-at-time "5sec" nil (lambda ()
;;                             (let ((fullscreen (frame-parameter (selected-frame) 'fullscreen)))
;;                               ;; If emacs has in fullscreen status, maximized window first, drag emacs window from Mac's single space.
;;                               (when (memq fullscreen '(fullscreen fullboth))
;;                                 (set-frame-parameter (selected-frame) 'fullscreen 'maximized))
;;                               ;; Call `toggle-frame-fullscreen' to fullscreen emacs.
;;                               (toggle-frame-fullscreen)))))

;; 窗口最大化
(add-hook 'after-init-hook 'toggle-frame-maximized)

;;======================== fringe 美化 =====================
(setq indicate-buffer-boundaries nil)
;; 不显示buffer末尾空行fringe
(setq indicate-empty-lines nil)
;; remove continuation arrow on right fringe
;; (delq! 'continuation fringe-indicator-alist 'assq)
(when (fboundp 'set-fringe-mode)
  (set-fringe-mode '(4 . 8)))
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

(unless (display-graphic-p)
  (setq overflow-newline-into-fringe nil))

;; doesn't exist in terminal Emacs; we define it to prevent errors
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))

;; =========================== minbuffer =============================
;;
;;; Minibuffer

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; _while_ we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
;; They're generally unhelpful and only add confusing visual clutter.
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;; =========================== minbuffer =============================

(use-package vi-tilde-fringe
  :if (fboundp 'set-fringe-mode)
  :diminish vi-tilde-fringe-mode
  :hook ((prog-mode text-mode conf-mode) . vi-tilde-fringe-mode))
;;======================== fringe 美化 =====================

(add-hook 'after-init-hook 'turn-on-visual-line-mode)

;; config built-in "display-line-numbers-mode" (require Emacs >= 26)
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode conf-mode protobuf-mode) . display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 3)
  (setq display-line-numbers-current-absolute t)
  (kevin/set-leader-keys "tn" 'display-line-numbers-mode))

;; 设置时间格式
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t))

(provide 'init-ui)
;;; init-ui ends here
