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

;; native comp
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p) (fboundp 'json-serialize))
  ;; fix libgccjit.so invoke gcc driver error
  (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin20/10.2.0")

  (let ((eln-cache-dir (expand-file-name "cache/eln/"
                                         user-emacs-directory))
        (find-exec (executable-find "find")))
    (setcar native-comp-eln-load-path eln-cache-dir)
    ;; Quitting emacs while native compilation in progress can leave zero byte
    ;; sized *.eln files behind. Hence delete such files during startup.
    (when find-exec
      (call-process find-exec nil nil nil eln-cache-dir
                    "-name" "*.eln" "-size" "0" "-delete" "-or"
                    "-name" "*.eln.tmp" "-size" "0" "-delete")))

  (setq package-native-compile t
        native-comp-speed 2
        native-comp-verbose 0
        native-comp-async-report-warnings-errors nil
        native-comp-async-jobs-number 4
        comp-deferred-compilation-deny-list
        '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)"
          ;; Don't native-compile *-authloads.el and *-pkg.el files as they
          ;; seem to produce errors during native-compile.
          "\\(?:[^z-a]*-autoloads\\.el$\\)"
          "\\(?:[^z-a]*-pkg\\.el$\\)")))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; no need of package-quickstart since we are using straight.el as package manager
(setq package-quickstart nil)
;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux)
  (setq command-line-x-option-alist nil))

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
