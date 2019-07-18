;; init.el -- emacs bootstrap file. -*- lexical-binding: t; -*-
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

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires %s or higher" minver)))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, Optimize loading performance
;;----------------------------------------------------------------------------
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq gc-cons-threshold (* 1024 1024 1024); 1G
      gc-cons-percentage 0.6)

;;----------------------------------------------------------------------------
;; be quiet at startup; don't load or display anything unnecessary
;;----------------------------------------------------------------------------
(advice-add #'display-startup-echo-area-message :override #'ignore)

;;----------------------------------------------------------------------------
;; Core files required.
;;----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'init-custom)
(require 'init-elpa)
(require 'init-const)
(require 'init-funcs)

;;----------------------------------------------------------------------------
;; Load custom file first.
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)

;;----------------------------------------------------------------------------
;; Add customized directories to load-path.
;;----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-osx)                    ; set up osx
(require 'init-evil)                   ; evil mode

;;----------------------------------------------------------------------------
;; personal package config
;;----------------------------------------------------------------------------
(require 'init-ui)
(require 'init-font)
(require 'init-dashboard)
(require 'init-highlight)
(require 'init-modeline)
;; (require 'init-awesome-tab)

;; misc packages
(require 'init-ivy)
(require 'init-chinese)
(require 'init-misc)
;; (require 'init-email)

;; programming releated packages
;; (require 'init-tag)
(require 'init-prog)
(require 'init-yasnippet)
(require 'init-company)
(require 'init-golang)
(require 'init-python)
(require 'init-markdown)
(require 'init-org)
(require 'init-elisp)
(require 'init-imenu)
(require 'init-persp-mode)
(require 'init-projectile)
(require 'init-lua)
(require 'init-rust)
(require 'init-cpp)
(require 'init-lsp)

;; tools
(require 'init-git)
(require 'init-buffer)
(require 'init-flycheck)
(require 'init-eshell)
(require 'init-dired)
(require 'init-filetree)
;; (require 'init-restore)

(require 'init-windows)
(require 'init-windows-popup)

(require 'init-keybinds)
(require 'init-better-default)         ; better defaluts

(add-hook 'emacs-startup-hook (lambda ()
                                "Restore defalut values after init"
                                (setq file-name-handler-alist default-file-name-handler-alist
                                      gc-cons-threshold (* 1024 1024 16) ; 16M
                                      gc-cons-percentage 0.1)))

;;; init.el ends here
