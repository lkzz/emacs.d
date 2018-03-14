;;; init-lsp.el --- Initialize language server
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :ensure t)

(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-cache-candidates t)
  (setq company-lsp-async t)
  (setq company-lsp-enable-snippet t))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-sideline-show-symbol t)
  (setq lsp-ui-sideline-enable nil))

(provide 'init-lsp)
;;; init-lsp ends here
