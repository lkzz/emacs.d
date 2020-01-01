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
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 400000000)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 8000000)

            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold 400000000))
            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold 8000000))
            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;;----------------------------------------------------------------------------
;; Add customized directories to load-path.
;;----------------------------------------------------------------------------
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "core" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path)
  (push (expand-file-name "vendor" user-emacs-directory) load-path))

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
(require 'init-elpa)
(require 'init-const)
(require 'init-funcs)

;;----------------------------------------------------------------------------
;; Load custom file
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)

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

;;; init.el ends here
