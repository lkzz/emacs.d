;;; buffer.el --- autoload functions used in buffer. -*- lexical-binding: t -*-
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
;;; Code:

;;;###autoload
(defun my-normal-buffer ()
  (or (not buffer-read-only)
      (buffer-file-name)))

;;;###autoload
(defun my-switch-to-next-buffer ()
  (interactive)
  (unless (minibufferp)
    (let ((p t) (bn (buffer-name)))
      (switch-to-next-buffer)
      (while (and p (not (my-normal-buffer)))
	    (switch-to-next-buffer)
	    (when (string= bn (buffer-name)) (setq p nil))))))

;;;###autoload
(defun my-switch-to-prev-buffer ()
  (interactive)
  (unless (minibufferp)
    (let ((p t) (bn (buffer-name)))
      (switch-to-prev-buffer)
      (while (and p (not (my-normal-buffer)))
	    (switch-to-prev-buffer)
	    (when (string= bn (buffer-name)) (setq p nil))))))

;;;###autoload
(defun my-revert-buffer-no-confirm ()
  "Revert buffer without confirm."
  (interactive)
  (revert-buffer t t))

;;;###autoload
(defun my-indent-region-or-buffer()
  "Indent regex or buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (point-min) (point-max)))))

;; Kill all buffers except scratch buffer
;;;###autoload
(defun my-kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

;; Kill all buffers except the current one.
;;;###autoload
(defun my-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;;###autoload
(defun my-create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (erase-buffer)
  (lisp-interaction-mode))

;;;###autoload
(defun my-rename-file()
  "Rename file while using current file as default."
  (interactive)
  (let ((file-from (read-file-name "Move from: " default-directory buffer-file-name))
        (file-to (read-file-name "Move to:" default-directory)))
    (rename-file file-from file-to)
    (when (string= (file-truename file-from) (file-truename (buffer-file-name)))
      (kill-buffer)
      (find-file file-to))))

;;;###autoload
(defun my-copy-file()
  "Copy file while using current file as default."
  (interactive)
  (copy-file
   (read-file-name "Copy from: " default-directory buffer-file-name)
   (read-file-name "Copy to:" default-directory)))

;;;###autoload
(defun my-delete-file()
  "Delete file while using current file as default."
  (interactive)
  (let ((file-name (read-file-name "Delete: " default-directory (buffer-file-name))))
    (cond
     ((file-directory-p file-name) (delete-directory file-name t))
     ((file-exists-p file-name) (delete-file file-name))
     (t (message "Not found!")))
    (unless (file-exists-p (buffer-file-name))
      (kill-current-buffer))))
