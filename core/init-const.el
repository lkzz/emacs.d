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

(provide 'init-const)
;;; init-const.el ends here
