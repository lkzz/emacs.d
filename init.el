;; init.el -- emacs bootstrap file. -*- lexical-binding: t; -*-
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

(setq gc-cons-threshold 100000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)))

;;----------------------------------------------------------------------------
;; be quiet at startup; don't load or display anything unnecessary
;;----------------------------------------------------------------------------
(advice-add #'display-startup-echo-area-message :override #'ignore)

;;----------------------------------------------------------------------------
;; Core files required.
;;----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'core)

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
(require 'init-exec-path) ; set up $PATH
(require 'init-evil)      ; evil mode
(require 'init-better-default)

;;----------------------------------------------------------------------------
;; personal package config
;;----------------------------------------------------------------------------
(require 'init-ui)
(require 'init-modeline)
;; (require 'init-kevin-modeline)
(require 'init-highlight)
(require 'init-filetree)

;; misc packages
(require 'init-anzu)
(require 'init-ivy)
(require 'init-chinese)
(require 'init-misc)
;; (require 'init-email)

;; programming releated packages
(require 'init-prog)
(require 'init-company)
(require 'init-golang)
(require 'init-python)
(require 'init-yasnippet)
(require 'init-markdown)
(require 'init-org)
(require 'init-elisp)
(require 'init-imenu)
(require 'init-persp-mode)
(require 'init-projectile)
(require 'init-lua)
;; (require 'init-lsp)

;; tools
(require 'init-git)
(require 'init-buffer)
(require 'init-flycheck)
(require 'init-dired)
(require 'init-ranger)
(require 'init-eshell)
(require 'init-restclient)
;; (require 'init-restore)

(require 'init-windows)
(require 'init-windows-popup)

(require 'init-keybinds)

;;; init.el ends here
