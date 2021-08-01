;;; core-package.el --- package install config. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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

;;------------------------------------------------------------------------------
;; use straight as package manager
;;------------------------------------------------------------------------------
(setq load-prefer-newer noninteractive)
;; init before load straight
(setq straight-base-dir kevin-cache-dir
      straight-cache-autoloads t
      straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-enable-package-integration nil
      straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t
      use-package-always-ensure nil)
;; load straight bootstrap file
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

(use-package diminish)
(use-package hydra)
(if (not (display-graphic-p))
    (use-package popup)
  (use-package posframe))
(use-package s
  :straight (:host github :repo "emacsmirror/s" :files (:defaults "*")))

;; Don't litter emacs directory
(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (expand-file-name "etc/" kevin-cache-dir))
  (setq no-littering-var-directory
        (expand-file-name "var/" kevin-cache-dir))
  (require 'no-littering)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package better-jumper
  :init
  (global-set-key [remap evil-jump-forward] #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (better-jumper-mode +1)
  (setq better-jumper-context 'window
        better-jumper-new-window-behavior 'copy
        better-jumper-add-jump-behavior 'replace
        better-jumper-max-length 100
        better-jumper-use-evil-jump-advice t))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package gcmh
  :diminish
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x1000000) ; 16MB
  (gcmh-mode 1))

(provide 'core-package)
;;; core-package.el ends here
