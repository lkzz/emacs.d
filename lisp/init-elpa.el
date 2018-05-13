;;; init-elps.el --- elpa config
;;; Commentary:
;;; Code:

(setq package-archives
      '(

        ;; ;; {{melpa repository:
        ;; ("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; ;; }}

        ;; ;; {{ 163 repository:
        ;; ("melpa" . "https://mirrors.163.com/elpa/melpa/")
        ;; ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")
        ;; ;; }}

        ;; {{ tsinghua repository (more stable than 163, recommended)
        ;;See https://mirror.tuna.tsinghua.edu.cn/help/elpa/ on usage:
        ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
        ;; ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ;; ;; }}

        ;; ;; {{ emacs-china repository:
        ;; ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
        ;; ;; }}

        ))


;;; Fire up package.el
(setq package-enable-at-startup nil)
(package-initialize)
;; 当el文件比elc文件新的时候,则加载el,即尽量Load最新文件文件
(setq load-prefer-newer t)

;;-----------------------------------------------------------------------------
;; install use-package
;;-----------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
;; (setq use-package-always-defer t)

;; Required by `use-package'
(use-package diminish
  :ensure t)

(use-package bind-key)
(use-package hydra)

(use-package general
  :config
  (general-evil-setup))

(provide 'init-elpa)
;;; init-elpa ends here
