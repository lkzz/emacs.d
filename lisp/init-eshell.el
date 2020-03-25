;;; init-eshell.el --- config eshell. -*- lexical-binding: t; -*-
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

;; https://github.com/manateelazycat/aweshell
(use-package aweshell
  :load-path "site-lisp/aweshell"
  :hook ((eshell-first-time-mode . kevin/eshell-keymap)
         (eshell-exit . delete-window))
  :init
  :config
  (setq eshell-highlight-prompt t
        eshell-prompt-function 'epe-theme-lambda
        eshell-directory-name (concat kevin-cache-dir "eshell/")))

(provide 'init-eshell)
;;; init-eshell.el ends here
