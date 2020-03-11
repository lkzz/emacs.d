;;; init-const.el --- const variables used in emacs config. -*- lexical-binding: t; -*-
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

(defconst is-windows-p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst is-linux-p
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst is-mac-p
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst is-emacs26-p
  (> emacs-major-version 25)
  "Emacs is 26 or above.")

(defconst is-emacs27-p
  (> emacs-major-version 26)
  "Emacs is 27 or above.")

(defconst kevin-cache-directory (expand-file-name "cache/" user-emacs-directory)
  "Emacs cache directory.")

(defconst kevin-leader-key "SPC"
  "The leader key.")

(defconst kevin-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defconst kevin-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defconst kevin-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defconst kevin-scratch-message (format ";; Happy Hacking, %s - Emacs ♥ You!\n" kevin-user-name))

(provide 'init-const)
;;; init-const.el ends here
