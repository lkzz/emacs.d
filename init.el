;; init.el -- emacs bootstrap file. -*- lexical-binding: t no-byte-compile: t -*-
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
;;; Code:

(when (version< emacs-version "28.1")
  (error "Your Emacs is too old -- this config requires 28.1 or higher"))

;; Adjust garbage collection thresholds during startup, Optimize loading performance
(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil
      site-run-file nil)

;; Optimize emacs garbage collect.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Hook run after loading init files
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist
                  gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;; Load core config of emacs
(load (concat user-emacs-directory "core/core") nil 'nomessage)
(my-initialize-core)

;; Be quiet at startup; don't load or display anything unnecessary
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Personal package config
(require 'init-evil)
(require 'init-ui)
;; (require 'init-dashboard)
(require 'init-highlight)

;; Misc packages
(require 'init-ivy)
(require 'init-chinese)
(require 'init-misc)
(require 'init-edit)
(require 'init-projectile)

;; Programming releated packages
(require 'init-org)
(require 'init-prog)
(require 'init-format)
(require 'init-golang)
(require 'init-python)
(require 'init-rust)
(require 'init-cxx)
(require 'init-markdown)
(require 'init-completion)
(require 'init-lsp)

;; Tools
(require 'init-vc)
(require 'init-buffer)
(require 'init-flycheck)
(require 'init-shell)
(require 'init-dired)
(require 'init-filetree)
(require 'init-window)
(require 'init-keybindings.el)

;;; init.el ends here
