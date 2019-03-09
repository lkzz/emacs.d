;;; init-eshell.el --- config eshell. -*- lexical-binding: t; -*-
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

(use-package aweshell
  :ensure nil
  :load-path "vendor/aweshell"
  :commands (aweshell-toggle)
  :hook (eshell-first-time-mode . kevin/eshell-keymap)
  :init
  (kevin/set-leader-keys "'" 'aweshell-toggle)
  :config
  (setq eshell-highlight-prompt t)
  (setq eshell-prompt-function 'epe-theme-lambda)
  (setq eshell-history-file-name (concat kevin-cache-directory "eshell/history")))

(provide 'init-eshell)
;;; init-eshell.el ends here
