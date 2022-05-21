;;; core.el --- emacs 核心配置. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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
(setq user-full-name "lkzz"
      user-mail-address "kevin.scnu@gmail.com")

(defconst is-windows-p
  (eq system-type 'windows-nt)
  "Are we running on a Win system?")

(defconst is-linux-p
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst is-mac-p
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst is-emacs26-p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst is-emacs27-p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst is-emacs28-p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst my-cache-dir (concat user-emacs-directory "cache/")
  "Emacs cache directory.")

(unless (file-exists-p my-cache-dir)
  (make-directory my-cache-dir))

(defconst my-autoload-file (concat my-cache-dir "core-autoloads.el")
  "This file is responsible for informing emacs where to find all autoload function in core/autoload/*.el")

(defconst my-http-proxy "127.0.0.1:1235"
  "Set http/https proxy.")

(defconst my-global-leader-prefix "SPC"
  "Global leader key prefix.")

(defconst my-local-leader-prefix ","
  "Local leader key prefix.")

(defconst my-lsp-backend 'eglot
  "Which language server to use, eglot, lsp-mode or lsp-bridge")

;; Ensure core dir is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless is-windows-p
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;; 简化yes-or-no 输入
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't autosave files or create lock/history/backup files.
(setq auto-save-default nil             ; 不生成 #filename# 临时文件
      create-lockfiles nil              ; 关闭lockfile,NOTE:有风险，建议开启
      make-backup-files nil             ; 关闭备份功能
      delete-by-moving-to-trash t)      ; 删除时移除到回收站

;; Misc
(setq confirm-nonexistent-file-or-buffer t ; Whether confirmation is requested before visiting a new file or buffer.
      confirm-kill-processes nil           ; kill running processes without confirmation on Emacs exit
      inhibit-compacting-font-caches t     ; gc 忽略字体缓存
      find-file-visit-truename t           ; 当是链接时，显示真正的连接
      uniquify-buffer-name-style 'forward)

;; Emacs "updates" its ui more often than it needs to, so we slow it down slightly.
(setq idle-update-delay 1)

;; Non-nil means reorder bidirectional text for display in the visual order.
;; Disabling this gives Emacs a tiny performance boost.
(setq-default bidi-display-reordering nil)
;; Optimize for very long lines
(setq bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)

;; Non-nil means highlight region even in nonselected windows.
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)
;; Make scrolling smoother by avoiding unnecessary fontification
(setq redisplay-skip-fontification-on-input t)

(defun my/initialize-core ()
  "Load core config file for Emacs."
  (require 'core-autoload)
  (require 'core-package)
  (require 'core-ui))

(provide 'core)
;;; core.el ends here
