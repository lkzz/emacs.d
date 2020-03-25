;;; window.el --- window releated functions. -*- lexical-binding: t -*-
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
(defun kevin/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;;;###autoload
(defun kevin/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;;;###autoload
(defun kevin/toggle-golden-ratio ()
  "Golden ratio mode toggle function."
  (interactive)
  (if golden-ratio-mode
      (progn
        (golden-ratio-mode -1)
        (message "golden ratio disabled")
        (balance-windows))
    (progn
      (golden-ratio-mode 1)
      (message "golden ratio enabled")
      (golden-ratio))))
