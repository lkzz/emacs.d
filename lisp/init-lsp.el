;; init-lsp.el --- Initialize lsp (Language Server Protocol) configurations. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; Commentary:
;; Code:

;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :if kevin-lsp-mode-enable-p
  :diminish lsp-mode
  :config
  (require 'lsp-imenu)
  (setq lsp-inhibit-message t)
  (setq lsp-message-project-root-warning t)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable))

(defun toggle-lsp-ui-doc ()
  (interactive)
  (if lsp-ui-doc-mode
      (progn
        (lsp-ui-doc-mode -1)
        (lsp-ui-doc--hide-frame))
    (lsp-ui-doc-mode 1)))

(defun my-lsp-mode-hook ()
  ;; delete-lsp-ui-doc frame is exists, and disable lsp-ui-doc by default
  (lsp-ui-doc--hide-frame)
  (lsp-ui-doc-mode -1))

(use-package lsp-ui
  :ensure t
  :after (:all markdown-mode lsp-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
	          ("C-c r ." . lsp-ui-peek-find-definitions)
	          ("C-c r ?" . lsp-ui-peek-find-references)
	          ("C-c r d" . lsp-ui-peek-find-definitions)
	          ("C-c r r" . lsp-ui-peek-find-references)
	          ("C-c r i" . lsp-ui-imenu)
	          ("C-c r F" . lsp-ui-sideline-apply-code-actions)
	          ("C-c r R" . lsp-rename))
  :config
  (setq scroll-margin 0)
  ;; (global-set-key (kbd "C-j") #'toggle-lsp-ui-doc)
  ;; (add-hook 'lsp-mode-hook #'my-lsp-mode-hook)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-sideline-ignore-duplicate t)
  (setq lsp-ui-sideline-show-symbol t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-flycheck t)
  (setq lsp-ui-sideline-enable t))

(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :config
  (cl-pushnew 'company-lsp company-backends)
  (setq company-lsp-async t)
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-cache-candidates t))

(use-package lsp-go
  :ensure t
  :after go-mode
  :commands lsp-go-enable
  :hook (go-mode . lsp-go-enable)
  :config (setq lsp-go-gocode-completion-enabled t))

(use-package lsp-python
  :ensure t
  :commands lsp-python-enable
  :hook (python-mode . lsp-python-enable)
  :config
  (setq-default flycheck-flake8-maximum-line-length 100))

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here
