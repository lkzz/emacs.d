;;; init-basic.el --- 优化emacs的默认配置. -*- lexical-binding: t; -*-
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

;; Personal information
(setq user-full-name kevin-user-name
      user-mail-address kevin-mail-address)

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please
(unless is-windows-p
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;; Indentation
(setq-default tab-width 4
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

;; Word wrapping
(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil)

;; Misc
(setq confirm-nonexistent-file-or-buffer t
      auto-save-default nil             ; 不生成 #filename# 临时文件
      make-backup-files nil             ; 关闭备份功能
      create-lockfiles nil              ; 关闭lockfile,NOTE:有风险，建议开启
      delete-by-moving-to-trash t       ; 删除时移除到回收站
      inhibit-compacting-font-caches t  ; gc 忽略字体缓存
      find-file-visit-truename t        ; 当是链接时，显示真正的连接
      uniquify-buffer-name-style 'forward)

;; 简化yes-or-no 输入
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil            ; Move line by visual line
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(provide 'init-basic)
;;; init-basic.el ends here
