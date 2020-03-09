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
(use-package "startup"
  :straight nil
  :init
  (setq inhibit-startup-screen t
        initial-buffer-choice  nil
        initial-scratch-message kevin-scratch-message
        inhibit-startup-echo-area-message t ; 禁止echo area message
        inhibit-default-init t              ; 禁止加载default lib
        initial-major-mode 'fundamental-mode)) ; 设置默认的major mode
(fset #'display-startup-echo-area-message #'ignore)

;; 禁止使用对话框
(setq use-file-dialog nil
      use-dialog-box nil)

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

(setq scroll-step 1
      scroll-margin 1
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      auto-window-vscroll nil
      fast-but-imprecise-scrolling nil
      mouse-wheel-scroll-amount '(1 ((shift) . 2))
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      scroll-preserve-screen-position t
      hscroll-step 1
      hscroll-margin 1)

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
  :straight nil
  :hook ((prog-mode text-mode conf-mode protobuf-mode) . display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 3)
  (setq display-line-numbers-current-absolute t))

;; 设置时间格式
(use-package time
  :straight nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t))

;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(use-package all-the-icons
  :demand
  :if (display-graphic-p)
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-playlist-mode all-the-icons-material "playlist_play" :height 1.2 :v-adjust -0.2 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :v-adjust -0.2 :face 'all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rss$" all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-list-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-item-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue)))

(use-package centaur-tabs
  :disabled
  :after evil
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 30
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs-left-edge-margin nil
        centaur-tabs-modified-marker "*"
        uniquify-separator "/"
        uniquify-buffer-name-style 'forward)
  (centaur-tabs-mode t)
  :general
  (kevin/colon-key-define
    "t" '(nil :which-key "Tab")
    "t n" 'centaur-tabs-forward-tab
    "t N" 'centaur-tabs-forward-group
    "t p" 'centaur-tabs-backward-tab
    "t P" 'centaur-tabs-backward-group
    "t a" 'centaur-tabs-select-beg-tab
    "t e" 'centaur-tabs-select-end-tab
    "t w" 'toggle-word-wrap)
  :hook ((dashboard-mode . centaur-tabs-local-mode)
         (dired-mode . centaur-tabs-local-mode)
         (eshell-mode . centaur-tabs-local-mode)
         (helpful-mode . centaur-tabs-local-mode)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20
        doom-modeline-bar-width 3
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-minor-modes t
        doom-modeline-buffer-encoding t
        doom-modeline-indent-info nil
        doom-modeline-enable-word-count t
        doom-modeline-buffer-modification-icon t
        doom-modeline-buffer-file-name-style 'auto))

(provide 'init-ui)
;;; init-ui ends here
