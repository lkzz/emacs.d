;;; init-cpp.el --- cpp config. -*- lexical-binding: t; -*-
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

(defun kevin/c-mode-common-setup ()
  (setq c-default-style "k&r")
  (setq c-basic-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)
  ;; syntax-highlight aggressively
  ;; (setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0)

  ;; make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1))

(add-hook 'c-mode-common-hook #'kevin/c-mode-common-setup)

(defun kevin/makefile-mode-setup ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(add-hook 'makefile-mode-hook #'kevin/makefile-mode-setup)

(use-package company-c-headers
  :after company
  :init (cl-pushnew 'company-c-headers company-backends))

(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(provide 'init-cpp)
;;; init-cpp.el ends here
