;; init-lsp.el --- Initialize lsp (Language Server Protocol) configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2020  Kevin Leung
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
  :diminish lsp-mode
  :commands lsp lsp-deferred
  :hook ((go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :general (lsp-mode-map "C-c C-d" 'lsp-describe-thing-at-point
                         "C-c C-n" 'lsp-rename)
  :init
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (setq lsp-session-file (concat kevin-cache-dir "lsp-session-v1")
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil

        ;; Disable eldoc displays in minibuffer
        lsp-eldoc-enable-hover nil

        lsp-modeline-diagnostics-enable nil
        lsp-modeline-code-actions-enable nil

        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil

        lsp-enable-folding nil
        lsp-enable-file-watchers nil
        lsp-enable-text-document-color nil
        lsp-enable-semantic-highlighting nil
        lsp-enable-symbol-highlighting nil)
  :config
  (use-package lsp-ui
    :custom-face
    (lsp-ui-sideline-code-action ((t (:inherit warning))))
    :general (lsp-ui-mode-map [remap evil-goto-definition] 'lsp-ui-peek-find-definitions
                              [remap xref-find-definitions] 'lsp-ui-peek-find-definitions
                              [remap xref-find-references] 'lsp-ui-peek-find-references)
    :init (setq lsp-ui-doc-enable t
                lsp-ui-doc-header nil
                lsp-ui-doc-use-webkit nil
                lsp-ui-doc-delay 0.5
                lsp-ui-doc-include-signature nil
                lsp-ui-doc-position 'at-point
                lsp-ui-doc-border (face-foreground 'font-lock-comment-face)

                lsp-ui-sideline-enable nil

                lsp-ui-imenu-enable t
                lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                      ,(face-foreground 'font-lock-string-face)
                                      ,(face-foreground 'font-lock-constant-face)
                                      ,(face-foreground 'font-lock-variable-name-face)))
    :config
    ;; Reset `lsp-ui-doc-background' after loading theme
    (add-hook 'after-load-theme-hook
              (lambda ()
                (setq lsp-ui-doc-border (face-foreground 'default))
                (set-face-background 'lsp-ui-doc-background
                                     (face-background 'tooltip))))

    ;; `C-g'to close doc
    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

  ;; Ivy integration
  (use-package lsp-ivy
    :after lsp-mode
    :bind (:map lsp-mode-map
                ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
                ("C-s-." . lsp-ivy-global-workspace-symbol)))

  )

(provide 'init-lsp)
;;; init-lsp.el ends here
