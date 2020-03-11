;; init.el -- emacs bootstrap file. -*- lexical-binding: t; -*-
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

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires %s or higher" minver)))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, Optimize loading performance
;;----------------------------------------------------------------------------
(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar custom-gc-cons-threshold 100000000)

(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8
      load-prefer-newer noninteractive
      site-run-file nil)

;; hook run after loading init files
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (setq file-name-handler-alist default-file-name-handler-alist
                                        gc-cons-threshold custom-gc-cons-threshold
                                        gc-cons-percentage 0.1)))

;; Optimize emacs garbage collect.
(add-hook 'minibuffer-setup-hook #'(lambda ()
                                     (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (garbage-collect)
                                    (setq gc-cons-threshold custom-gc-cons-threshold)))
(add-hook 'focus-out-hook #'garbage-collect)

;;----------------------------------------------------------------------------
;; Add customized directories to load-path.
;;----------------------------------------------------------------------------
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "core" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

;;----------------------------------------------------------------------------
;; be quiet at startup; don't load or display anything unnecessary
;;----------------------------------------------------------------------------
(advice-add #'display-startup-echo-area-message :override #'ignore)

;;----------------------------------------------------------------------------
;; Load core files first.
;;----------------------------------------------------------------------------
(require 'init-custom)
(require 'init-package)
(require 'init-const)
(require 'init-funcs)
(require 'init-keybind)

;;----------------------------------------------------------------------------
;; Load custom file
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)

;;----------------------------------------------------------------------------
;; basic config
;;----------------------------------------------------------------------------
(require 'init-basic)
(require 'init-osx)
(require 'init-evil)

;;----------------------------------------------------------------------------
;; personal package config
;;----------------------------------------------------------------------------
(require 'init-ui)
(require 'init-font)
;;(require 'init-dashboard)
(require 'init-highlight)
;; (require 'init-modeline)

;; misc packages
(require 'init-ivy)
(require 'init-chinese)
(require 'init-misc)
(require 'init-edit)

;; programming releated packages
(require 'init-prog)
(require 'init-yasnippet)
(require 'init-company)
(require 'init-golang)
(require 'init-python)
(require 'init-markdown)
(require 'init-org)
(require 'init-elisp)
(require 'init-projectile)
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
(require 'init-window)
(require 'init-shackle)

;;; init.el ends here
