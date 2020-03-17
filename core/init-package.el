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
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))
;; Fire up package.el
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil
        ;; don't add that `custom-set-variables' block to my initl!
        package--init-file-ensured t) ; don't auto-initialize!
  (package-initialize))
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file we load.
(setq load-prefer-newer noninteractive)
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
  (setq use-package-enable-imenu-support t))

(eval-when-compile
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
