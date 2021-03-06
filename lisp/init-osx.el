;;; init-osx.el --- special config for mac osx. -*- lexical-binding: t; -*-
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


(when is-mac-p
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-right-option-modifier 'nil
        mac-right-option-modifier 'super)
  (global-set-key [(super a)] 'mark-whole-buffer)
  (global-set-key [(super v)] 'yank)
  (global-set-key [(super c)] 'kill-ring-save)
  (global-set-key [(super s)] 'save-buffer)
  (global-set-key [(super w)] (lambda () (interactive) (delete-window)))
  (global-set-key [(super z)] 'undo))

(use-package exec-path-from-shell
  :if is-mac-p
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(provide 'init-osx)
;;; init-osx ends here
