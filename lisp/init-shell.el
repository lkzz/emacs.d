;;; init-shell.el --- config shell. -*- lexical-binding: t; -*-
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

(use-package vterm
  :config
  (add-hook 'vterm-mode-hook (lambda ()
                               (evil-set-initial-state 'vterm-mode 'emacs)
                               (advice-add #'vterm--redraw :after (lambda (&rest args)
                                                                    (evil-refresh-cursor evil-state))))))

(use-package vterm-toggle
  :general (my-space-leader-def "t '" 'vterm-toggle)
  :after vterm)

(provide 'init-shell)
