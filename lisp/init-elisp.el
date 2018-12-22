;;; init-elisp.el --- Initialize emacs lisp. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;

;;; Commentary:
;;; Code:

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
  :defer t
  :hook (emacs-lisp-mode . remove-elc-on-save))

(use-package elisp-def
  :ensure t
  :diminish elisp-def-mode ""
  :hook (emacs-lisp-mode . elisp-def-mode))

;; Show function arglist or variable docstring
(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :hook (after-init . global-eldoc-mode))

;; This library adds all of the familiar highlighting to cl-lib macros
(use-package cl-lib-highlight
  :defer t
  :config
  (cl-lib-highlight-initialize))

(provide 'init-elisp)
;;; init-elisp.el ends here
