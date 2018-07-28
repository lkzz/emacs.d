;;; core-elpa.el --- elpa config. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(setq package-archives
      '(

        ;; ;; {{melpa repository:
        ;; ("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; ;; }}

        ;; emacs-china repository:
        ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
        ("gnu-cn" . "http://elpa.emacs-china.org/gnu/")
        ("org-cn" . "http://elpa.emacs-china.org/org/")

        ))


;;; Fire up package.el
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my initl!
      package--init-file-ensured t)

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

(use-package diminish)
(use-package bind-map)
(use-package bind-key)
(use-package hydra)
(use-package general)

(use-package which-key
  :defer t
  :ensure t
  :diminish which-key-mode "ⓦ"
  :commands (which-key-add-major-mode-key-based-replacements
              which-key-add-key-based-replacements)
  :hook (after-init . which-key-mode)
  :config
  (progn
    (setq which-key-idle-delay 0.4)
    (setq which-key-side-window-max-width 0.33)
    (setq which-key-side-window-max-height 0.25)
    (setq which-key-allow-imprecise-window-fit t) ; performance
    (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
    (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
    (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
    (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))))
