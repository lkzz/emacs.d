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

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "k&r"
        c-basic-offset 4))

(use-package company-c-headers
  :after company
  :init (cl-pushnew 'company-c-headers company-backends))

(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(provide 'init-cpp)
;;; init-cpp.el ends here
