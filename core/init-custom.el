;;; init-custom.el --- define variables can be customized. -*- lexical-binding: t; -*-
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

(defgroup kevin-customize-group nil
  "Variables can be customized in my emacs.")

(defcustom kevin-user-name "kevin"
  "Default username."
  :type 'string
  :group 'kevin-customize-group)

(defcustom kevin-mail-address "kevin.scnu@gmail.com"
  "Default email address."
  :type 'string
  :group 'kevin-customize-group)

(defcustom kevin-theme-selected 'molokai
  "Set color theme."
  :type '(choice
          (const :tag "zenburn theme" zenburn)
          (const :tag "monokai theme" monokai)
          (const :tag "doom one theme" doom)
          (const :tag "doom tomorrow theme" tomorrow)
          (const :tag "gruvbox theme" gruvbox)
          (const :tag "doom molokai thme") molokai)
  :group 'kevin-customize-group)

(defcustom kevin-lsp-mode-enable-p nil
  "Enable lsp mode or not."
  :type 'boolean
  :group 'kevin-customize-group)

(provide 'init-custom)
;;; init-custom ends here
