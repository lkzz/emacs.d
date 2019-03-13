;;; init-elisp.el --- Initialize emacs lisp. -*- lexical-binding: t; -*-
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

(kevin/define-jump-handlers emacs-lisp-mode)

;;;###autoload
(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(use-package elisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . remove-elc-on-save))

(use-package elisp-def
  :diminish elisp-def-mode ""
  :hook (emacs-lisp-mode . elisp-def-mode))

;; Show function arglist or variable docstring
(use-package eldoc
  :diminish eldoc-mode
  :hook (after-init . global-eldoc-mode))

;; This library adds all of the familiar highlighting to cl-lib macros
(use-package cl-lib-highlight
  :config
  (cl-lib-highlight-initialize))

(provide 'init-elisp)
;;; init-elisp.el ends here
