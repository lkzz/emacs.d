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
  :commands kevin/toggle-aweshell
  :hook ((eshell-first-time-mode . kevin/eshell-keymap)
         (eshell-exit . delete-window))
  :init
  (defun kevin/toggle-aweshell ()
    (interactive)
    (split-window (frame-root-window) (kevin/calculate-window-height) 'below)
    (other-window 1)
    (aweshell-toggle))
  :config
  (defvar eshell-window-height 35
    "Percentage for shell-buffer window height.")

  (defun kevin/calculate-window-height ()
    (let* ((win (frame-root-window))
           (size (window-height win)))
      (round (* size (/ (- 100 eshell-window-height) 100.0)))))

  (defun kevin/quit-or-delete-char (arg)
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp nil))
        (eshell-life-is-too-much)
      (delete-char arg)))

  (defun kevin/ivy-eshell-history ()
    (interactive)
    (let* ((start-pos (save-excursion (eshell-bol) (point)))
           (end-pos (point))
           (input (buffer-substring-no-properties start-pos end-pos))
           (command (ivy-read "Command: "
                              (delete-dups
                               (when (> (ring-size eshell-history-ring) 0)
                                 (ring-elements eshell-history-ring)))
                              :initial-input input)))
      (setf (buffer-substring start-pos end-pos) command)
      (end-of-line)))

  (defun kevin/eshell-keymap ()
    (evil-define-key 'insert eshell-mode-map
      (kbd "C-u") 'eshell-kill-input
      (kbd "C-a") 'eshell-bol
      (kbd "C-d") 'kevin/quit-or-delete-char
      (kbd "C-r") 'kevin/ivy-eshell-history
      (kbd "TAB") 'pcomplete-std-complete))

  (setq eshell-highlight-prompt t
        eshell-prompt-function 'epe-theme-lambda
        eshell-directory-name (concat kevin-cache-dir "eshell/")))


(provide 'init-eshell)
;;; init-eshell.el ends here
