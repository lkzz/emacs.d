;;; init-format.el --- code format config. -*- lexical-binding: t -*-
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

;; FIXME: 只在evil insert mode下开启，normal mode会莫名跳到行首
;; issue: https://github.com/lassik/emacs-format-all-the-code/issues/173
(defun my/enable-format-before-save ()
  (format-all-ensure-formatter)
  (add-hook 'before-save-hook (lambda ()
                                (when (eq evil-state 'insert)
                                  (format-all-buffer)))))

;; Set up 'format-all' to format on save
(use-package format-all
  :defer t
  :commands format-all-buffer
  :config
  (format-all-ensure-formatter))

(use-package aggressive-indent-mode
  :defer t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(provide 'init-format)
;;; init-format.el ends here
