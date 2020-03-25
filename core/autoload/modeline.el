;;; modeline.el --- autoload function used in modeline. -*- lexical-binding: t -*-
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

;; fix icon background color
;; https://github.com/domtronn/all-the-icons.el/issues/131
;;;###autoload
(defun kevin/propertize-icon (icon)
  (add-face-text-property
   0 (length icon)
   :inherit t icon)
  icon)

;;;###autoload
(defun kevin/maybe-alltheicon (&rest args)
  "Display octicon via `ARGS'."
  (when (display-graphic-p)
    (kevin/propertize-icon (apply 'all-the-icons-alltheicon args))))

;;;###autoload
(defun kevin/maybe-faicon-icon (&rest args)
  "Display font awesome icon via `ARGS'."
  (when (display-graphic-p)
    (kevin/propertize-icon (apply 'all-the-icons-faicon args))))

;;;###autoload
(defun shorten-directory (dir max-length)
  "Setup a directory(`DIR') `MAX-LENGTH' characters."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))
