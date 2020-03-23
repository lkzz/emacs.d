;;; eshell.el --- autoload functions in eshell mode. -*- lexical-binding: t -*-
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
;;; Code:

;;;###autoload
(defun kevin/calculate-window-height ()
  (let* ((win (frame-root-window))
         (size (window-height win)))
    (round (* size (/ (- 100 kevin-eshell-height) 100.0)))))

;;;###autoload
(defun kevin/quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (eshell-life-is-too-much)
    (delete-char arg)))

;;;###autoload
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

;;;###autoload
(defun kevin/eshell-keymap ()
  (evil-define-key 'insert eshell-mode-map
    (kbd "C-u") 'eshell-kill-input
    (kbd "C-a") 'eshell-bol
    (kbd "C-d") 'kevin/quit-or-delete-char
    (kbd "C-r") 'kevin/ivy-eshell-history
    (kbd "TAB") 'pcomplete-std-complete))

;;;###autoload
(defun kevin/toggle-aweshell ()
  (interactive)
  (split-window (frame-root-window) (kevin/calculate-window-height) 'below)
  (other-window 1)
  (aweshell-toggle))
