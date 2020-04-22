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
         (c++-mode . lsp-deferred))
  :general (lsp-mode-map "C-c C-d" 'lsp-describe-thing-at-point
                         "C-c C-r" 'lsp-ui-peek-find-references
                         "C-c C-n" 'lsp-rename
                         "C-c C-." 'lsp-ui-peek-find-definitions
                         [remap evil-goto-definition] 'lsp-find-definition
                         [remap xref-find-definitions] 'lsp-find-definition
                         [remap xref-find-references] 'lsp-find-references)
  :init
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (setq lsp-session-file (concat kevin-cache-dir "lsp-session-v1")
        ;;prefer flycheck
        lsp-diagnostic-package :flycheck
        lsp-auto-guess-root t
        ;;disable file wathcer when large file
        lsp-enable-file-watchers nil
        ;; enable log only for debug
        lsp-log-io nil
        ;; completion
        lsp-prefer-capf nil
        ;; turn off for better performance
        lsp-enable-symbol-highlighting nil
        ;; Disable eldoc displays in minibuffer
        lsp-eldoc-enable-hover nil
        lsp-flycheck-live-reporting nil
        ;; auto kill server
        lsp-keep-workspace-alive nil)
  :config
  (use-package lsp-ui
    :custom-face
    (lsp-ui-sideline-code-action ((t (:inherit warning))))
    :init (setq lsp-ui-doc-enable t
                lsp-ui-doc-use-webkit nil
                lsp-ui-doc-delay 0.2
                lsp-ui-doc-include-signature t
                lsp-ui-doc-position 'at-point
                lsp-ui-doc-border (face-foreground 'default)
                lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer
                lsp-ui-sideline-enable nil
                lsp-ui-imenu-enable t
                lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                      ,(face-foreground 'font-lock-string-face)
                                      ,(face-foreground 'font-lock-constant-face)
                                      ,(face-foreground 'font-lock-variable-name-face)))
    :config
    (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

    ;; `C-g'to close doc
    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

    ;; Reset `lsp-ui-doc-background' after loading theme
    (add-hook 'after-load-theme-hook
              (lambda ()
                (setq lsp-ui-doc-border (face-foreground 'default))
                (set-face-background 'lsp-ui-doc-background
                                     (face-background 'tooltip))))

    ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
    (defun my-lsp-ui-imenu-hide-mode-line ()
      "Hide the mode-line in lsp-ui-imenu."
      (setq mode-line-format nil))
    (advice-add #'lsp-ui-imenu :after #'my-lsp-ui-imenu-hide-mode-line))

  ;; Complete
  (use-package company-lsp
    :init
    (setq company-lsp-async t
          company-lsp-enable-snippet t
          company-lsp-cache-candidates 'auto
          company-lsp-enable-recompletion t)))

(provide 'init-lsp)
;;; init-lsp.el ends here
