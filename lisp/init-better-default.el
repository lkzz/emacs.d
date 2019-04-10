;;; init-better-default.el --- 些常用的琐碎的配置，应该在init.el的最后加载. -*- lexical-binding: t; -*-
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

;; Personal information
(setq user-full-name kevin-user-name
      user-mail-address kevin-mail-address)


;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; default directory
(setq default-directory kevin-default-directory)

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system 'utf-8)          ; pretty
(setq selection-coding-system 'utf-8)  ; pretty
(setq locale-coding-system 'utf-8)     ; please

;; 复制粘贴
(setq select-enable-primary t
      select-enable-clipboard t)

(setq-default indent-tabs-mode nil ;; do not insert tab indentation
              tab-width 4 ;; 将TAB显示为4个空格.
              fill-column 80 ;; 设置列宽度
              buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              grep-highlight-matches t
              grep-scroll-output t
              line-spacing 0
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              truncate-partial-width-windows nil
              truncate-lines nil           ; Do not display continuation lines
              split-height-threshold nil   ; Disable vertical window splitting
              split-width-threshold nil    ; Disable horizontal window splitting
              majar-mode 'text-mode)

;; 禁止显示警告提示
(setq visible-bell nil)
;; 关闭警告提示音
(setq ring-bell-function 'ignore)
;; 一键删除选择区域
(delete-selection-mode t)
;; 简化yes-or-no 输入
(defalias 'yes-or-no-p 'y-or-n-p)
;; 关闭备份功能
(setq make-backup-files nil)
;; 关闭自动保存模式
(setq auto-save-list-file-prefix
      (concat kevin-cache-directory "auto-save-list/.saves-"))
(setq-default auto-save-mode nil)
;; 不生成 #filename# 临时文件
(setq auto-save-default nil)
;; 关闭lockfile,NOTE:有风险，建议开启
(setq create-lockfiles nil)
;; 当使用emacs时触发垃圾回收
(add-hook 'focus-out-hook #'garbage-collect)

;; 自动刷新文件
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package url
  :disabled
  :ensure nil
  :config
  (setq url-configuration-directory (concat kevin-cache-directory "url")))

;; Keep cursor at end of lines. Require line-move-visual is nil.
(setq track-eol t)
(setq line-move-visual nil)

;; (mouse-avoidance-mode 'animate)

;; 当鼠标移动的时候自动转换frame，window或者minibuffer
(setq mouse-autoselect-window t)
;; 关闭像素滚动
(setq mac-mouse-wheel-smooth-scroll nil)

;; 鼠标滚动设置
(setq scroll-step 3
      scroll-margin 3
      scroll-conservatively 100000
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3 ((shift) . 3)))

;; 文件末尾插入新行
(setq require-final-newline t
      next-line-add-newlines nil)
(define-key global-map (kbd "RET") 'newline-and-indent)

;;删除时移到回收站
(setq delete-by-moving-to-trash t)

;; Control use of local variables in files you visit.
;; :safe means set the safe variables, and ignore the rest.
(setq enable-local-variables :safe)

(provide 'init-better-default)
;;; init-better-default.el ends here
