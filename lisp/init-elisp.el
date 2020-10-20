;;; init-elisp.el --- Initialize emacs lisp. -*- lexical-binding: t; -*-
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

(use-package elisp-mode
  :ensure nil
  :config
  ;; Show function arglist or variable docstring
  (use-package eldoc
    :diminish eldoc-mode)

  ;; This library adds all of the familiar highlighting to cl-lib macros
  (use-package cl-lib-highlight
    :config
    (cl-lib-highlight-initialize))

  (add-hook #'before-save-hook
            (lambda ()
              (when (and (eq major-mode 'emacs-lisp-mode)
                         (buffer-modified-p))
                (indent-region (point-min) (point-max))))))

(provide 'init-elisp)
;;; init-elisp.el ends here
