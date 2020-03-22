;;; buffer.el --- autoload functions used in buffer. -*- lexical-binding: t -*-
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
(defun kevin/normal-buffer ()
  (or (not buffer-read-only)
      (buffer-file-name)))

;;;###autoload
(defun kevin/switch-to-next-buffer ()
  (interactive)
  (unless (minibufferp)
    (let ((p t) (bn (buffer-name)))
      (switch-to-next-buffer)
      (while (and p (not (kevin/normal-buffer)))
	    (switch-to-next-buffer)
	    (when (string= bn (buffer-name)) (setq p nil))))))

;;;###autoload
(defun kevin/switch-to-prev-buffer ()
  (interactive)
  (unless (minibufferp)
    (let ((p t) (bn (buffer-name)))
      (switch-to-prev-buffer)
      (while (and p (not (kevin/normal-buffer)))
	    (switch-to-prev-buffer)
	    (when (string= bn (buffer-name)) (setq p nil))))))

;;;###autoload
(defun kevin/revert-buffer-no-confirm ()
  "Revert buffer without confirm."
  (interactive)
  (revert-buffer t t))

;;;###autoload
(defun indent-buffer()
  "Indent buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun kevin/indent-region-or-buffer()
  "Indent regex or buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (Indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

;; Kill all buffers except scratch buffer
;;;###autoload
(defun kevin/kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

;; Kill all buffers except the current one.
;;;###autoload
(defun kevin/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;;###autoload
(defun kevin/create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (erase-buffer)
  (lisp-interaction-mode))

;;;###autoload
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a 'before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;;;###autoload
(defun kevin/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))
