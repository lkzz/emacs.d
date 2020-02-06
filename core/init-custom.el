;;; init-custom.el --- define variables can be customized. -*- lexical-binding: t; -*-
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

(defcustom kevin-theme-selected 'doom-gruvbox
  "Customize color theme."
  :type '(choice
          (const :tag "light theme" doom-solarized-light)
          (const :rag "dark theme"  doom-one)
          (const :tag "hard dark theme" doom-gruvbox))
  :group 'kevin-customize-group)

;; Add yasnippet support for all company backends.
(defcustom kevin-enable-company-yasnippet t
  "Enable yasnippet for all backends."
  :type 'boolean
  :group 'kevin-customize-group)

(provide 'init-custom)
;;; init-custom ends here
