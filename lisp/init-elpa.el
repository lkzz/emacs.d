;;; init-elps.el --- elpa config
;;; Commentary:
;;; Code:

(setq package-archives '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
                           ("org-cn"   . "http://elpa.emacs-china.org/org/")
                           ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

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
(setq use-package-enable-imenu-support t)

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)


(provide 'init-elpa)
;;; init-elpa ends here
