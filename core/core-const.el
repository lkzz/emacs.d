;;; core-const.el --- const variable. -*- lexical-binding: t -*-
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
;;; Code:

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

(defconst my-lsp-backend 'lsp-bridge
  "Which language server to use, eglot, lsp-mode or lsp-bridge")

(provide 'core-const)
;;; core-const.el ends here
