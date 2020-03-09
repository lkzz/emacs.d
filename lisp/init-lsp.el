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
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                          (lsp-deferred))))
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)
                       ;; Format and organize imports
                       (add-hook 'before-save-hook #'lsp-format-buffer t t)
                       (add-hook 'before-save-hook #'lsp-organize-imports t t))))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("C-c C-r" . lsp-ui-peek-find-references)
              ("C-c C-n" . lsp-rename)
              ("C-c C-." . lsp-ui-peek-find-definitions)
              ([remap evil-goto-definition] . lsp-find-definition)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init (setq lsp-auto-guess-root t
              lsp-prefer-flymake nil
              lsp-keep-workspace-alive nil
              lsp-enable-symbol-highlighting nil
              flymake-fringe-indicator-position 'right-fringe
              lsp-file-watch-threshold 5000
              lsp-session-file (concat kevin-cache-directory "lsp-session-v1")
              lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  :config
  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom-face
    (lsp-ui-sideline-code-action ((t (:inherit warning))))
    :init (setq lsp-ui-doc-enable t
                lsp-ui-doc-use-webkit nil
                lsp-ui-doc-delay 0.5
                lsp-ui-doc-include-signature t
                lsp-ui-doc-position 'at-point
                lsp-ui-doc-border (face-foreground 'default)
                lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer
                lsp-ui-sideline-enable t
                lsp-ui-sideline-show-hover nil
                lsp-ui-sideline-show-diagnostics nil
                lsp-ui-sideline-ignore-duplicate t
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
          company-lsp-enable-recompletion t))

  ;; C/C++/Objective-C support
  (use-package ccls
    :defines projectile-project-root-files-top-down-recurring
    :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
    :config
    (with-eval-after-load 'projectile
      (setq projectile-project-root-files-top-down-recurring
            (append '("compile_commands.json" ".ccls")
                    projectile-project-root-files-top-down-recurring)))
    (setq ccls-executable "ccls"
          ccls-initialization-options `(:cache (:directory "/tmp/ccls-cache"),
                                               :compilationDatabaseDirectory "build"))))

(provide 'init-lsp)
;;; init-lsp.el ends here
