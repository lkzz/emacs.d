;;; core-variables.el --- variables used in emacs config file. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:


;; -----------------------------------------------------------------------------
;; const variables
;; -----------------------------------------------------------------------------
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst kevin/cache-directory (expand-file-name "cache/" user-emacs-directory)
  "Emacs cache directory.")

(defconst kevin/default-directory "~/.emacs.d"
  "Default project directory.")

(defvar kevin/leader-key "SPC"
  "The leader key.")

(defvar kevin/emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar kevin/major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar kevin/major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar kevin/default-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defvar kevin/user-name "kevin"
  "Default username.")

(defvar kevin/mail-address "kevin.scnu@gmail.com"
  "Default email address.")

(defgroup kevin nil
  "Declare group used in my emacs."
  :group 'kevin)

(defcustom color-theme 'dark
  "Set color theme."
  :type '(choice
          (const :tag "zenburn theme" default)
          (const :tag "doom theme" dark)
          (const :tag "gruvbox theme" gruvbox)
          (const :tag "Leuven light theme" light))
  :group 'kevin)

(defcustom enable-lsp nil
  "Enable lsp mode or not."
  :type 'boolean)


(defvar kevin/scratch-message (format ";; Happy Hacking, %s - Emacs ♥ You!\n" kevin/user-name))

(byte-recompile-file "~/.emacs.d/core/core-variables.el" nil 0)
(provide 'core-variables)
;;; core-variables.el ends here
