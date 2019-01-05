;;; core-elpa.el --- elpa config. -*- lexical-binding: t; -*-
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

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

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
(setq use-package-verbose t)
;; (setq use-package-always-defer t)
(use-package use-package-ensure-system-package)

(use-package diminish :ensure t)
(use-package bind-map :ensure t)
(use-package bind-key :ensure t)
(use-package hydra :ensure t)
(use-package posframe :ensure t)

(use-package which-key
  :demand t
  :ensure t
  :diminish which-key-mode "ⓦ"
  :commands (which-key-add-major-mode-key-based-replacements
              which-key-add-key-based-replacements)
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-min-display-lines 1)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-display-columns nil)
  (setq which-key-sort-uppercase-first nil)
  (setq which-key-side-window-max-width 0.33)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-allow-imprecise-window-fit t) ; performance
  (setq which-key-sort-order #'which-key-prefix-then-key-order)
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))


;; (byte-recompile-file "~/.emacs.d/core/core-elpa.el" nil 0)
(provide 'core-elpa)
;;; core-elpa.el ends here
