;;; init-ui.el ---  setup ui. -*- lexical-binding: t; -*-
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

(setq use-file-dialog nil)
(setq use-dialog-box nil)

(when kevin-mac-p
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
;; 显示buffer末尾空行fringe
(setq indicate-empty-lines t)
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
(setq inhibit-startup-screen t)
;; 设置scratch buffer message
(setq initial-scratch-message kevin-scratch-message)
;; 高亮当前行
(global-hl-line-mode t)
;; 设置光标形状
(setq-default cursor-type '(bar . 3))
;; 设置光标颜色
(add-to-list 'default-frame-alist '(cursor-color . "red"))
;; 禁止光标闪烁
(blink-cursor-mode -1)
;; 禁止响铃
(setq ring-bell-function 'ignore)
;; 标题栏格式设置
(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))
;; 打开文件时不再创建新的frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; 安装常用的主题
(use-package doom-themes)
(use-package gruvbox-theme)
;; (use-package spacemacs-theme)
;; 加载主题
(defun kevin/load-customized-theme (theme)
  "Load customized THEME."
  (interactive)
  ;; whether the theme is a doom theme
  (when (string-prefix-p "doom" (symbol-name theme))
    (doom-themes-org-config)
    (doom-themes-neotree-config)
    (doom-themes-visual-bell-config))
  (load-theme theme t))
(kevin/load-customized-theme kevin-theme-selected)

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

;; fringe 美化,left fringe with 4 pixel ,right fringe width:8 pixel
(set-fringe-mode '(4 . 8))
;; 设置visual line fringe bitmap
(when (fboundp 'define-fringe-bitmap)
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

(add-hook 'after-init-hook 'turn-on-visual-line-mode)

(use-package vi-tilde-fringe
  :diminish vi-tilde-fringe-mode
  :hook ((prog-mode text-mode conf-mode) . vi-tilde-fringe-mode))

;; config built-in "display-line-numbers-mode" (require Emacs >= 26)
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 2)
  ;; (setq-default display-line-numbers-type 'relative)
  (setq display-line-numbers-current-absolute t)
  (kevin/set-leader-keys "tn" 'display-line-numbers-mode))

(use-package hide-mode-line
  :hook (neotree-mode . hide-mode-line-mode))

(provide 'init-ui)
;;; init-ui ends here
