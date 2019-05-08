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
  :if kevin-lsp-mode-enable-p
  :diminish lsp-mode "ⓛ"
  :commands lsp
  :hook ((go-mode . lsp)
         (rust-mode . lsp)
         (c++-mode . lsp))
  :config
  (setq lsp-enable-xref t
        lsp-enable-snippet t
        lsp-auto-guess-root t
        lsp-inhibit-message t
        lsp-prefer-flymake nil
        lsp-enable-indentation t
        lsp-enable-symbol-highlighting nil
        lsp-eldoc-render-all nil
        lsp-session-file (concat kevin-cache-directory "lsp-session-v1")
        ;; lsp go client
        lsp-clients-go-server "gopls"
        lsp-clients-go-format-tool "goimports"
        lsp-clients-go-use-binary-pkg-cache t
        lsp-clients-go-func-snippet-enabled t
        lsp-clients-go-max-parallelism 2
        lsp-clients-go-gocode-completion-enabled nil))

(use-package lsp-ui
  :if kevin-lsp-mode-enable-p
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
	          ("C-c r d" . lsp-ui-peek-find-definitions)
	          ("C-c r r" . lsp-ui-peek-find-references)
	          ("C-c r i" . lsp-ui-imenu)
	          ("C-c r f" . lsp-ui-sideline-apply-code-actions)
	          ("C-c r n" . lsp-rename))
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
  :if kevin-lsp-mode-enable-p
  :after (company lsp-mode)
  :commands company-lsp
  :config
  (setq company-lsp-async t
        company-lsp-enable-snippet t
        company-lsp-cache-candidates nil
        company-lsp-enable-recompletion t))

(provide 'init-lsp)
;;; init-lsp.el ends here
