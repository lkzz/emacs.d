;;; init-elisp.el --- Initialize emacs lisp.
;;; Commentary:
;;; Code:


(use-package elisp-mode
  :ensure nil
  :config
  (defun remove-elc-on-save ()
    "If you're saving an elisp file, likely the .elc is no longer valid."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda ()
                (if (file-exists-p (concat buffer-file-name "c"))
                    (delete-file (concat buffer-file-name "c"))))))
  (add-hook 'emacs-lisp-mode-hook #'kevin/remove-elc-on-save))

;; Show function arglist or variable docstring
(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :init
  ;; Enable Eldoc in lisp modes in 24
  ;; `global-eldoc-mode' is enabled by default in 25.
  (unless (fboundp 'global-eldoc-mode)
    (dolist (hook '(emacs-lisp-mode-hook
                    lisp-interaction-mode-hook
                    ielm-mode-hook
                    eval-expression-minibuffer-setup-hook))
      (add-hook hook #'eldoc-mode))))

;; This library adds all of the familiar highlighting to cl-lib macros
(use-package cl-lib-highlight
  :ensure t
  :config
  (cl-lib-highlight-initialize))

(provide 'init-elisp)
;;; init-elisp.el ends here
