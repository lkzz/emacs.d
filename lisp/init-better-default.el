;;; init-better-default.el --- 一些常用的琐碎的配置，应该在init.el的最后加载
;;; Commentary:

;;; Code:

;; Personal information
(setq user-full-name "kevin leung")
(setq user-mail-address "kevin.scnu@gmail.com")

;; default directory
(setq default-directory "~/Code/gopath/src")

;; 设置编码
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; 复制粘贴
(setq select-enable-primary t)
(setq select-enable-clipboard t)


(setq-default indent-tabs-mode t ;; 用空格替代TAB
              default-tab-width 4 ;; 定义TAB的宽度为4个空格.
              fill-column 80 ;; 设置列宽度
              buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode nil
              line-spacing 0
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              truncate-lines nil
              truncate-partial-width-windows nil)

;; 禁止显示警告提示
(setq visible-bell nil)
;; 关闭警告提示音
(setq ring-bell-function 'ignore)
;; 一键删除选择区域
(delete-selection-mode t)
;; 简化yes-or-no 输入
(fset 'yes-or-no-p 'y-or-n-p)
;; 关闭备份功能
(setq make-backup-files nil)
;; 关闭自动保存模式
(setq-default auto-save-mode nil)
;; 不生成 #filename# 临时文件
(setq auto-save-default nil)
;; 关闭lockfile,NOTE:有风险，建议开启
(setq create-lockfiles nil)
;; 自动刷新文件
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :init (add-hook 'after-init-hook #'global-auto-revert-mode))

;; 显示文件大小信息
(when (fboundp size-indication-mode)
  (size-indication-mode t))

;; Keep cursor at end of lines. Require line-move-visual is nil.
(setq track-eol t)
(setq line-move-visual nil)

;; 当
(mouse-avoidance-mode 'animate)
;; 当鼠标移动的时候自动转换frame，window或者minibuffer
(setq mouse-autoselect-window t)
;; 鼠标滚动设置
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 3
      scroll-margin 3
      scroll-conservatively 100000)
(use-package smooth-scrolling
  :init (add-hook 'after-init-hook #'smooth-scrolling-mode)
  :config (setq smooth-scroll-margin 0))

;; 文件末尾插入新行
(setq require-final-newline t)
(setq next-line-add-newlines nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

;;删除时移到回收站
(setq delete-by-moving-to-trash t)

(provide 'init-better-default)
;;; init-better-default.el ends here
