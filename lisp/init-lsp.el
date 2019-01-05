;; init-lsp.el --- Initialize lsp (Language Server Protocol) configurations. -*- lexical-binding: t; -*-
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

(use-package lsp-mode
  :ensure t
  :if kevin-lsp-mode-enable-p
  :diminish lsp-mode
  :config
  (require 'lsp-imenu)
  (setq create-lockfiles nil)
  (setq lsp-inhibit-message t)
  (setq lsp-message-project-root-warning t)
  (setq lsp-hover-text-function 'lsp--text-document-signature-help)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)

  (defun lsp-restart-server ()
    "Restart LSP server."
    (interactive)
    (lsp-restart-workspace)
    (revert-buffer t t)
    (message "LSP server restarted."))

  (use-package lsp-ui
    :ensure t
    :hook (lsp-mode . lsp-ui-mode)
    :bind (:map lsp-ui-mode-map
                ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                ([remap xref-find-references] . lsp-ui-peek-find-references)
	            ("C-c r d" . lsp-ui-peek-find-definitions)
	            ("C-c r r" . lsp-ui-peek-find-references)
	            ("C-c r i" . lsp-ui-imenu)
	            ("C-c r F" . lsp-ui-sideline-apply-code-actions)
	            ("C-c r R" . lsp-rename))
    :config
    (setq scroll-margin 0)
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-flycheck-enable t)
    (setq lsp-ui-imenu-enable t)
    (setq lsp-ui-sideline-ignore-duplicate t)
    (setq lsp-ui-sideline-show-symbol t)
    (setq lsp-ui-sideline-show-hover t)
    (setq lsp-ui-sideline-show-flycheck t)
    (setq lsp-ui-sideline-enable t)

    (use-package company-lsp
      :ensure t
      :after (company lsp-mode)
      :config
      (cl-pushnew 'company-lsp company-backends)
      (setq company-lsp-async t)
      (setq company-lsp-enable-snippet t)
      (setq company-lsp-cache-candidates t))

    ;; Go support for lsp-mode using Sourcegraph's Go Language Server
    ;; Install: go get -u github.com/sourcegraph/go-langserver
    (use-package lsp-go
      :ensure t
      :after go-mode
      :commands lsp-go-enable
      :hook (go-mode . lsp-go-enable)
      :config
      (setq lsp-ui-flycheck-enable nil)
      (setq lsp-go-gocode-completion-enabled t))


    (use-package pyenv-mode
      :ensure t
      :config
      (python-mode))

    ;; Python support for lsp-mode using pyls.
    ;; Install: pip install python-language-server
    (use-package lsp-python
      :ensure t
      :after python-mode
      :commands lsp-python-enable
      :hook (python-mode . lsp-python-enable)
      ;; :config
      ;; (setq-default flycheck-flake8-maximum-line-length 100)
      )
    ;; Bash support for lsp-mode using Mads Hartmann's bash-language-server
    ;; Install: npm i -g bash-language-server@1.4.0
    ;; Require Python2.5+, use --python to specify.
    (use-package lsp-sh
      :commands lsp-sh-enable
      :hook (sh-mode . lsp-sh-enable))

    ))
(provide 'init-lsp)
;;; init-lsp.el ends here
