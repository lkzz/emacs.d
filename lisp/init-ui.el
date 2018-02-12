;;; init-ui.el ---  setup ui
;;; Commentary:
;;; Code:

(setq use-file-dialog nil)
(setq use-dialog-box nil)

; ;; Show a marker in the left fringe for lines not in the buffer
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
(use-package color-theme-sanityinc-tomorrow
  :init
  (add-hook 'after-init-hook (lambda ()
                               (load-theme 'sanityinc-tomorrow-night t))))

;; ;; 配置主题
;; (use-package doom-themes
;;   :init
;;   (add-hook 'after-init-hook (lambda ()
;;                                (load-theme 'doom-tomorrow-night t)))
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; 字体设置
(use-package cnfonts
  :init
  (add-hook 'after-init-hook #'cnfonts-enable)
  :config
  (setq cnfonts-keep-frame-size nil)
  (setq cnfonts-use-cache t)
  (setq cnfonts-profiles
        '("program" "org-mode" "read-book"))
  (setq cnfonts-directory
        (concat kevin/cache-directory "cnfonts"))
  (setq cnfonts--profiles-steps '(("program" . 5)
                                  ("org-mode" . 5)
                                  ("read-book" . 8)))
  )

;; fringe 美化
;; 更改边缘的厚度（默认为8像素）
(defvar fringe-size '4
  "Default fringe width.")
(if (fboundp 'fringe-mode) (fringe-mode fringe-size))
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

(use-package fringe-helper
  :commands (fringe-helper-define fringe-helper-convert)
  :init
  (unless (fboundp 'define-fringe-bitmap)
    ;; doesn't exist in terminal Emacs; define it to prevent errors
    (defun define-fringe-bitmap (&rest _)))
  :after git-gutter-fringe
  :config
  (progn
    ;; places the git gutter outside the margins.
    (setq-default fringes-outside-margins t)
    ;; thin fringe bitmaps
    (fringe-helper-define 'git-gutter-fr:added '(center repeated)
                          "XXX.....")
    (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
                          "XXX.....")
    (fringe-helper-define 'git-gutter-fr:deleted 'bottom
                          "X......."
                          "XX......"
                          "XXX....."
                          "XXXX...."))
  )

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :demand t
  :config
  (progn
    ;; If you enable global minor mode
    (set-face-foreground 'git-gutter-fr:modified "cyan3")
    (set-face-foreground 'git-gutter-fr:added    "SeaGreen3")
    (set-face-foreground 'git-gutter-fr:deleted  "orchid3")
    (add-hook 'after-init-hook 'global-git-gutter-mode)))

(use-package vi-tilde-fringe
  :hook ((prog-mode text-mode conf-mode) . vi-tilde-fringe-mode))

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :init (add-hook 'prog-mode-hook #'display-line-numbers-mode))
  (use-package linum-off
    :demand
    :init (add-hook 'after-init-hook #'global-linum-mode)
    :config (setq linum-format "%4d ")))

(use-package nyan-mode
  :init (add-hook 'after-init-hook #'nyan-mode)
  :config
  (progn
    (setq nyan-wavy-trail t)
    (setq nyan-animate-nyancat t)))

(provide 'init-ui)
;;; init-ui ends here
