;;; core.el --- emacs 核心配置. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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

(defvar my-first-input-hook nil)
(add-hook 'pre-command-hook #'(lambda ()
                                (when my-first-input-hook
                                  (run-hooks 'my-first-input-hook)
                                  (setq my-first-input-hook nil))))


(defvar my/load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme'.")
(defun my/run-load-theme-hooks (&rest _)
  (run-hooks 'my/load-theme-hook))
(advice-add #'load-theme :after #'my/run-load-theme-hooks)

(add-hook 'my/load-theme-hook #'window-divider-mode)

;; Ensure core dir is in `load-path'
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(defun my/initialize-core ()
  "Load core config file for Emacs."
  (require 'core-const)
  (require 'core-basic)
  (require 'core-package))

(provide 'core)
;;; core.el ends here
