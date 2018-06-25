;; init-lsp.el --- Initialize lsp (Language Server Protocol) configurations.
;; Commentary:
;; Code:

;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :diminish lsp-mode)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :commands (lsp-ui-mode lsp-ui-peek-find-definistions lsp-ui-peek-find-references)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  (progn
    (setq lsp-ui-sideline-enable  nil)
    ))

(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :init (cl-pushnew 'company-lsp company-backends))

;; Go support for lsp-mode using Sourcegraph's Go Language Server
;; Install: go get github.com/sourcegraph/go-langserver
(use-package lsp-go
  :commands lsp-go-enable
  :init (add-hook 'go-mode-hook #'lsp-go-enable))

;; Python support for lsp-mode using pyls.
;; Install: pip install python-language-server
(use-package lsp-python
  :commands lsp-python-enable
  :init (add-hook 'python-mode-hook #'lsp-python-enable))

;; Javascript, Typescript and Flow support for lsp-mode
;; Install: npm i -g javascript-typescript-langserver
(use-package lsp-javascript-typescript
  :commands lsp-javascript-typescript-enable
  :init
  (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable))

;; Java support for lsp-mode using the Eclipse JDT Language Server.
;; Install:
;; wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
;; tar jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
(use-package lsp-java
  :commands lsp-java-enable
  :init (add-hook 'java-mode-hook #'lsp-java-enable))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
