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
  :if kevin-lsp-mode-enable-p
  :diminish lsp-mode
  :config
  (setq lsp-inhibit-message t)
  (setq lsp-message-project-root-warning t)
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

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
  :if kevin-lsp-mode-enable-p
  :after lsp-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq scroll-margin 0)
  ;; overwrite s-j key for toggle-lsp-ui-doc
  (global-set-key (kbd "C-j") #'toggle-lsp-ui-doc)
  (add-hook 'lsp-mode-hook #'my-lsp-mode-hook))

(use-package company-lsp
  :ensure t
  :if kevin-lsp-mode-enable-p
  :after (company lsp-mode)
  :init (cl-pushnew 'company-lsp company-backends))

;; Go support for lsp-mode using Sourcegraph's Go Language Server
;; Install: go get -u github.com/sourcegraph/go-langserver
(use-package lsp-go
  :ensure t
  :ensure-system-package
  (go-langserver . "go get -u github.com/sourcegraph/go-langserver")
  :if kevin-lsp-mode-enable-p
  :after (go-mode lsp-mode)
  :commands lsp-go-enable
  :hook (go-mode . lsp-go-enable)
  :config (setq lsp-go-gocode-completion-enabled t))

(provide 'init-lsp)
;;; init-lsp.el ends here
