;;; init-ui.el ---  setup ui
;;; Commentary:
;;; Code:

(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Show a marker in the left fringe for lines not in the buffer
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
;; 高亮当前行
(global-hl-line-mode t)
;; 设置光标形状
(setq-default cursor-type '(bar . 3))
;; 设置光标颜色
;; (add-to-list 'default-frame-alist '(cursor-color . "red"))
;; 禁止光标闪烁
(blink-cursor-mode -1)
;; 禁止响铃
(setq ring-bell-function 'ignore)
;; 标题栏格式设置
(setq frame-title-format
      '("" " Kevin "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; 设置scratch message
(setq initial-scratch-message "")
;; 打开文件时不再创建新的frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; 启动时窗口最大化
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; 配置主题
(require 'color-theme-tomorrow)
(load-theme 'tomorrow-night t)

;; 字体设置
;; Auto generated by cnfonts
;; <https://github.com/tumashu/cnfonts>
(set-face-attribute
 'default nil
 :font (font-spec :name "-*-Consolas-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                  :weight 'normal
                  :slant 'normal
                  :size 15.0))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font
   (frame-parameter nil 'font)
   charset
   (font-spec :name "-*-Microsoft YaHei-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
              :weight 'normal
              :slant 'normal
              :size 16.0)))


;; fringe 美化
;; left fringe with 4 pixel ,right fringe width:8 pixel
(if (fboundp 'fringe-mode) (fringe-mode '(4 . 8)))

;; 设置visual line fringe bitmap
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'right-curly-arrow
    [#b00000000
     #b00000000
     #b00000000
     #b00000000
     #b01110000
     #b00010000
     #b00010000
     #b00000000])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00000000
     #b00001000
     #b00001000
     #b00001110
     #b00000000
     #b00000000
     #b00000000
     #b00000000])
  (set-fringe-bitmap-face 'right-curly-arrow 'warning)
  (set-fringe-bitmap-face 'left-curly-arrow 'warning)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))
(add-hook 'after-init-hook 'turn-on-visual-line-mode)

(use-package vi-tilde-fringe
  :ensure t
  :diminish vi-tilde-fringe-mode
  :defer t
  :hook ((prog-mode text-mode conf-mode) . vi-tilde-fringe-mode))

(use-package nlinum
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'text-mode-hook 'nlinum-mode)
  :config
  (setq nlinum-format "%4d "))

;; (use-package nyan-mode
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'after-init-hook #'nyan-mode)
;;   :config
;;   (progn
;;     (setq nyan-wavy-trail t)
;;     (setq nyan-animate-nyancat t)))


(provide 'init-ui)
;;; init-ui ends here
