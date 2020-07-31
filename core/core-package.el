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
;; package bootstrap
;;------------------------------------------------------------------------------
(defun kevin-ensure-package ()
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

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  ;; Should set before loading `use-package'
  (eval-and-compile
    (setq use-package-always-ensure t)
    (setq use-package-expand-minimally t)
    (setq use-package-enable-imenu-support t)))

;;-----------------------------------------------------------------------------
;; straight bootstrap
;;-----------------------------------------------------------------------------
(defun kevin-ensure-straight ()
  ;; enable http proxy: 127.0.0.1:1235
  (kevin/enable-http-proxy)
  (setq straight-repository-branch "develop"
        straight-use-package-by-default t
        straight-check-for-modifications nil
        straight-vc-git-default-clone-depth 1
        straight-enable-package-integration nil
        straight-cache-autoloads t)

  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package))

(kevin-ensure-straight)

(eval-when-compile
  (require 'use-package))

;;-----------------------------------------------------------------------------
;; install use-package
;;-----------------------------------------------------------------------------
(use-package diminish)
(use-package posframe)
(use-package hydra)

(provide 'core-package)
;;; core-package.el ends here
