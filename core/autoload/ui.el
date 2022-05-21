;;; ui.el --- insert description here -*- lexical-binding: t -*-
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
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;;;###autoload
(defun my/toggle-darkroom-mode ()
  (interactive)
  (darkroom-tentative-mode (if darkroom-tentative-mode 0 1))
  (if darkroom-tentative-mode
      (add-hook 'window-configuration-change-hook 'darkroom-tentative-mode)
    (remove-hook 'window-configuration-change-hook 'darkroom-tentative-mode)))
