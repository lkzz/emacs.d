;;; init-ui.el ---  setup ui. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;
;;; Code:

(setq use-file-dialog nil)
(setq use-dialog-box nil)
;; 打开抗锯齿
(setq mac-allow-anti-aliasing t)
;; 关闭srgb，修复modeline上的颜色显示问题
(setq ns-use-srgb-colorspace nil)
;; 去除全屏时的黑边
(setq frame-resize-pixelwise t)

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
(add-to-list 'default-frame-alist '(cursor-color . "red"))
;; 禁止光标闪烁
(blink-cursor-mode -1)
;; 禁止响铃
(setq ring-bell-function 'ignore)
;; 标题栏格式设置
(setq frame-title-format
      '("" " Kevin "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; natural title bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; 设置scratch message
(setq initial-scratch-message "")
;; 打开文件时不再创建新的frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; 启动时窗口最大化
(add-hook 'after-init-hook 'toggle-frame-maximized)

;; 配置主题
(cond
 ;; default theme
 ((eq color-theme 'default)
  (use-package zenburn-theme
    :ensure t
    :config
    (load-theme 'zenburn t)))
 ;; dark theme
 ((eq color-theme 'dark)
  (use-package doom-themes
    :config
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (doom-themes-org-config)
    (load-theme 'doom-molokai t)))
 ((eq color-theme 'light)
  (use-package doom-themes
    :config
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (doom-themes-org-config)
    (load-theme 'doom-solarized-light t)))
 ((eq color-theme 'solarized)
  (use-package color-theme-solarized
    :ensure t
    :config
    (set-frame-parameter nil 'background-mode 'dark)
    (set-terminal-parameter nil 'background-mode 'dark)
    (setq solarized-broken-srgb t)
    (load-theme 'solarized t))))

;; 字体设置
(use-package cnfonts
  :defer t
  :preface
  ;; Fallback to `all-the-icons'.
  (defun cnfonts--set-all-the-icons-fonts (&optional _)
    "Show icons in all-the-icons."
    (when (featurep 'all-the-icons)
      (dolist (charset '(kana han cjk-misc bopomofo gb18030))
        (set-fontset-font "fontset-default" charset "all-the-icons" nil 'append)
        (set-fontset-font "fontset-default" charset "github-octicons" nil 'append)
        (set-fontset-font "fontset-default" charset "FontAwesome" nil 'append)
        (set-fontset-font "fontset-default" charset "Material Icons" nil 'append)
        (set-fontset-font "fontset-default" charset "file-icons" nil 'append)
        (set-fontset-font "fontset-default" charset "Weather Icons" nil 'append))))
  :hook ((after-init . cnfonts-enable)
         (cnfonts-set-font-finish . cnfonts--set-all-the-icons-fonts))
  :config
  ;; NOTE: on macOS, the frame size is changed during the startup without below.
  ;; Keep frame size
  (setq cnfonts-keep-frame-size nil)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq cnfonts-keep-frame-size t)))
  ;; Set profiles
  (setq cnfonts-directory (concat kevin/cache-directory "cnfonts"))
  (setq cnfonts-use-cache t)
  (setq cnfonts-profiles
        '("program1" "program2" "program3" "org-mode" "read-book"))
  (setq cnfonts--profiles-steps '(("program1" . 4)
                                  ("program2" . 5)
                                  ("program3" . 3)
                                  ("org-mode" . 6)
                                  ("read-book" . 8))))

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
  :defer t
  :diminish vi-tilde-fringe-mode
  :defer t
  :hook ((prog-mode text-mode conf-mode) . vi-tilde-fringe-mode))

;; config built-in "display-line-numbers-mode" (require Emacs >= 26)
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  :init
  (progn
    (setq-default display-line-numbers-width 2)
    ;; (setq-default display-line-numbers-type 'relative)
    (setq display-line-numbers-current-absolute t)
    (kevin/set-leader-keys "tn" 'display-line-numbers-mode)))

;; (use-package nyan-mode
;;   :defer t
;;   :ensure t
;;   :init (add-hook 'after-init-hook #'nyan-mode)
;;   :config
;;   (progn
;;     (setq nyan-wavy-trail t)
;;     (setq nyan-animate-nyancat t)))

;; Beacon flashes the cursor whenever you adjust position.
(use-package beacon
  :defer t
  :diminish beacon-mode
  :init (beacon-mode 1)
  :config
  (setq beacon-color "red")
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode))


(use-package vimish-fold
  :defer t
  :bind (:map vimish-fold-folded-keymap ("<tab>" . vimish-fold-unfold)
              :map vimish-fold-unfolded-keymap ("<tab>" . vimish-fold-refold))
  :init
  (progn
    (setq-default vimish-fold-dir (concat kevin/cache-directory "/vimish-fold"))
    (defhydra hydra-vimish-fold (:color pink)
      "
    ^
    ^Fold^              ^Do^                ^Jump^              ^Toggle^
    ^────^──────────────^──^────────────────^────^──────────────^──────^────────────
    _q_ quit            _f_ fold            _<_ previous        _<tab>_ current
    ^^                  _k_ kill            _>_ next            _S-<tab>_ all
    ^^                  _K_ kill all        ^^                  ^^
    ^^                  ^^                  ^^                  ^^
    "
      ("q" nil)
      ("<tab>" vimish-fold-toggle)
      ("S-<tab>" vimish-fold-toggle-all)
      ("<" vimish-fold-previous-fold)
      (">" vimish-fold-next-fold)
      ("f" vimish-fold)
      ("k" vimish-fold-delete)
      ("K" vimish-fold-delete-all)))
  :config
  (progn
    (vimish-fold-global-mode 1)
    (setq-default vimish-fold-header-width 79)))

(use-package posframe
  :defer t
  :ensure t)

(provide 'init-ui)
;;; init-ui ends here
