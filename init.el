;; init.el -- emacs bootstrap file. -*- lexical-binding: t; -*-
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

(when (version< emacs-version "27.1")
  (error "Your Emacs is too old -- this config requires 27.1 or higher"))

;; Adjust garbage collection thresholds during startup, Optimize loading performance
(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar custom-gc-cons-threshold 100000000)

(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8
      load-prefer-newer noninteractive
      site-run-file nil)

;; Hook run after loading init files
(add-hook 'emacs-startup-hook (lambda ()
                                (setq file-name-handler-alist default-file-name-handler-alist
                                      gc-cons-threshold custom-gc-cons-threshold
                                      gc-cons-percentage 0.1)))

;; Optimize emacs garbage collect.
(add-hook 'minibuffer-setup-hook (lambda ()
                                   (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook (lambda ()
                                  (garbage-collect)
                                  (setq gc-cons-threshold custom-gc-cons-threshold)))
(add-function :after after-focus-change-function (lambda ()
                                                   (unless (frame-focus-state)
                                                     (garbage-collect))))

;; Load the heart of emacs
(load (concat user-emacs-directory "core/core") nil 'nomessage)
(kevin/initialize-core)

;; Be quiet at startup; don't load or display anything unnecessary
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Personal package config
(require 'init-osx)
(require 'init-evil)
(require 'init-ui)
(require 'init-dashboard)
(require 'init-highlight)

;; Misc packages
(require 'init-ivy)
(require 'init-chinese)
(require 'init-misc)
(require 'init-edit)
(require 'init-projectile)

;; Programming releated packages
(require 'init-company)
(require 'init-yasnippet)
(require 'init-lsp)
(require 'init-org)
(require 'init-prog)
(require 'init-golang)
(require 'init-markdown)
(require 'init-python)
(require 'init-rust)
(require 'init-cxx)
(require 'init-elisp)
(require 'init-web)

;; Tools
(require 'init-vc)
(require 'init-buffer)
(require 'init-flycheck)
(require 'init-eshell)
(require 'init-dired)
(require 'init-filetree)
(require 'init-window)
(require 'init-shackle)

;;; init.el ends here
