;;; init.el --- This file bootstraps the configuration, which is divided into
;;; a number of other files.

;;; Commentary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((minver "24.3"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "24.5")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time


(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1014 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))


;;----------------------------------------------------------------------------
;; custom file.
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;----------------------------------------------------------------------------
;; be quiet at startup; don't load or display anything unnecessary
;;----------------------------------------------------------------------------
(advice-add #'display-startup-echo-area-message :override #'ignore)

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-funcs)
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; personal package config
;;----------------------------------------------------------------------------
;; ui setup
(require 'init-color-theme)
(require 'init-nlinum)
(require 'init-ui)
(require 'init-modeline)
(require 'init-highlight)
;; (require 'init-treemacs)
(require 'init-whitespace)

(require 'init-anzu)
(require 'init-recentf)
(require 'init-company)
(require 'init-ivy)
(require 'init-mwim)
(require 'init-chinese)
(require 'init-evil)

;; programming set up
(require 'init-programming)
(require 'init-golang)
(require 'init-yasnippet)
(require 'init-markdown)

(require 'init-projectile)
(require 'init-git)
(require 'init-which-key)
(require 'init-windows)
(require 'init-persp-mode)
(require 'init-flycheck)
(require 'init-dired)
(require 'init-eshell)

(require 'init-better-default)
(require 'init-keybinds)

;;; init.el ends here
