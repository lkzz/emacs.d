;;; init-elps.el --- elpa config
;;; Commentary:
;;; Code:

;; (require 'cl)
;; (require 'package)

(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			 ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
			 ; ("melpa" . "https://melpa.org/packages/")
             ))

(defun require-package (package)
  "Install PACKAGE unless already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful."
  (condition-case err
      (require-package package)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

;;; Fire up package.el
(setq package-enable-at-startup nil)
(package-initialize)

;;-----------------------------------------------------------------------------
;; install use-package
;;-----------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)


(require-package 'anzu)
(require-package 'company)
(require-package 'git-blamed)
(require-package 'go-mode)
(require-package 'company-go)
(require-package 'go-eldoc)
(require-package 'go-projectile)
(require-package 'gotest)
(require-package 'toml-mode)
(require-package 'ivy)
(require-package 'swiper)
(require-package 'projectile)
(require-package 'counsel)
(require-package 'markdown-mode)
(require-package 'mwim)
(require-package 'nlinum)
(require-package 'smex)
(require-package 'switch-window)
(require-package 'window-numbering)
(require-package 'which-key)


(provide 'init-elpa)

;;; init-elpa ends here
