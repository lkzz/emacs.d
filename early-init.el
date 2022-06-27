;;; early-init.el. -*- lexical-binding: t no-byte-compile: t -*-
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
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

;; Native comp
(when (fboundp 'native-comp-available-p)
  ;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
  ;; packages are compiled ahead-of-time when they are installed and site files
  ;; are compiled when gccemacs is installed.
  (setq native-comp-deferred-compilation nil)
  ;; Suppress native compilation warnings.
  (setq native-comp-async-report-warnings-errors nil))

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; No need of package-quickstart since we are using straight.el as package manager
(setq package-quickstart nil)
;; Do not initialise the package manager. This is done in `init.el'.
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (bound-and-true-p tooltip-mode) (tooltip-mode -1))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Make UTF-8 the default coding system:
(set-language-environment "UTF-8")
