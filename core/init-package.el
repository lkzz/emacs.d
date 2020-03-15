;;; init-package.el --- elpa config. -*- lexical-binding: t; -*-
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

;;-----------------------------------------------------------------------------
;; package-initialize
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("org"   . "http://elpa.emacs-china.org/org/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;;; Fire up package.el
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my initl!
      package--init-file-ensured t)
(package-initialize)
;; 当el文件比elc文件新的时候,则加载el,即尽量Load最新文件文件
(setq load-prefer-newer t)
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; install use-package
;;-----------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (require 'use-package))
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; install use-package
;;-----------------------------------------------------------------------------
(use-package diminish)
(use-package bind-key)
(use-package posframe)
(use-package hydra)

(provide 'init-package)
;;; init-package.el ends here
