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
  :diminish lsp-mode "ⓛ"
  :commands lsp
  :init
  (add-hook 'go-mode-hook #'lsp)
  :config
  (setq lsp-enable-xref t
        lsp-enable-snippet t
        lsp-auto-guess-root t
        lsp-inhibit-message t
        lsp-prefer-flymake nil
        lsp-enable-indentation t
        lsp-eldoc-render-all nil
        lsp-session-file (concat kevin-cache-directory "lsp-session-v1")
        ;; lsp go client
        lsp-clients-go-format-tool "goimports"
        lsp-clients-go-use-binary-pkg-cache t
        lsp-clients-go-func-snippet-enabled nil
        lsp-clients-go-gocode-completion-enabled t))

(use-package lsp-ui
  :ensure t
  :if kevin-lsp-mode-enable-p
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
	          ("C-c r d" . lsp-ui-peek-find-definitions)
	          ("C-c r r" . lsp-ui-peek-find-references)
	          ("C-c r i" . lsp-ui-imenu)
	          ("C-c r F" . lsp-ui-sideline-apply-code-actions)
	          ("C-c r R" . lsp-rename))
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-doc-enable nil
        lsp-ui-imenu-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t))

(use-package company-lsp
  :ensure t
  :if kevin-lsp-mode-enable-p
  :after (company lsp-mode)
  :commands company-lsp
  :config
  (setq company-lsp-async t
        company-lsp-enable-snippet nil
        company-lsp-cache-candidates t
        company-lsp-enable-recompletion nil))

(provide 'init-lsp)
;;; init-lsp.el ends here
