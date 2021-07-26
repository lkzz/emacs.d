;;; init-eshell.el --- config eshell. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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
  :commands aweshell-toggle
  :straight (aweshell :host github :repo "manateelazycat/aweshell")
  :hook ((eshell-first-time-mode . kevin/eshell-keymap)
         (eshell-exit . delete-window))
  :general (my-space-leader-def "t '" '(kevin/toggle-aweshell :wk "aweshell"))
  :config
  (setq eshell-highlight-prompt t
        eshell-prompt-function 'epe-theme-lambda))

(provide 'init-eshell)
;;; init-eshell.el ends here
