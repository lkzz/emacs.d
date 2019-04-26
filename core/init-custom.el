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

(defcustom kevin-theme-selected 'doom-molokai
  "Customize color theme."
  :type '(choice
          (const :tag "light theme" doom-solarized-light)
          (const :rag "dark theme"  doom-molokai)
          (const :tag "hard dark theme" gruvbox-hard-dark))
  :group 'kevin-customize-group)

(defcustom kevin-lsp-mode-enable-p t
  "Enable lsp mode or not."
  :type 'boolean
  :group 'kevin-customize-group)

(provide 'init-custom)
;;; init-custom ends here
