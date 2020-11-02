;;; core-package.el --- package install config. -*- lexical-binding: t; -*-
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

;;------------------------------------------------------------------------------
;; Package bootstrap
;;------------------------------------------------------------------------------
(setq  package-user-dir (concat kevin-cache-dir "elpa/")
       package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                          ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;; Fire up package.el
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (require 'use-package))

;; Use quelpa package manager
(setq quelpa-dir (concat kevin-cache-dir "quelpa/")
      quelpa-checkout-melpa-p nil       ; only use quelpa install package not in melpa
      quelpa-update-melpa-p nil         ; disable auto upgrade
      quelpa-melpa-recipe-stores nil    ; diable stores default recipes for package
      quelpa-self-upgrade-p nil)        ; diable self upgrade
(require 'quelpa)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)
(quelpa-use-package-activate-advice)

(use-package diminish)
(use-package posframe)
(use-package hydra)

;; Don't litter emacs directory
(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (expand-file-name "etc/" kevin-cache-dir))
  (setq no-littering-var-directory
        (expand-file-name "var/" kevin-cache-dir))
  (require 'no-littering)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(provide 'core-package)
;;; core-package.el ends here
