;;; init-shell.el --- config shell. -*- lexical-binding: t; -*-
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

(use-package vterm
  :defer t
  :init
  (setq vterm-always-compile-module t)
  (add-hook 'vterm-mode-hook (lambda ()
                               (advice-add #'vterm--redraw :after (lambda (&rest args)
                                                                    (evil-refresh-cursor evil-state))))))

(use-package vterm-toggle
  :defer t)

(provide 'init-shell)
