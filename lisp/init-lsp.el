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
  :diminish lsp-mode "ⓛ"
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                         (lsp-deferred))))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("C-c C-r" . lsp-ui-peek-find-references)
              ("C-c C-n" . lsp-rename)
              ("C-c C-." . lsp-ui-peek-find-definitions))
  :init (setq lsp-auto-guess-root t
              lsp-prefer-flymake nil
              flymake-fringe-indicator-position 'right-fringe)
  :config
  (setq lsp-enable-xref t
        lsp-enable-snippet t
        lsp-inhibit-message t
        lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 5000
        lsp-session-file (concat kevin-cache-directory "lsp-session-v1"))

  ;; Configure LSP clients
  (use-package lsp-clients
    :ensure nil
    :hook (go-mode . (lambda ()
                       "Format and add/delete imports."
                       (add-hook 'before-save-hook #'lsp-format-buffer t t)
                       (add-hook 'before-save-hook #'lsp-organize-imports t t)))
    :init
    (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
    (unless (executable-find "rls")
      (setq lsp-rust-rls-server-command '("rustup" "run" "stable" "rls"))))

  (use-package lsp-ui
    :functions my-lsp-ui-imenu-hide-mode-line
    :commands lsp-ui-doc-hide
    :custom-face
    (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
    (lsp-ui-sideline-code-action ((t (:inherit warning))))
    :bind (:map lsp-ui-mode-map
                ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                ([remap xref-find-references] . lsp-ui-peek-find-references))
    :init (setq lsp-ui-doc-enable nil
                lsp-ui-doc-use-webkit nil
                lsp-ui-doc-delay 0.5
                lsp-ui-doc-include-signature nil
                lsp-ui-doc-position 'at-point
                lsp-ui-doc-border (face-foreground 'default)
                lsp-eldoc-enable-hover nil ; Disableeldoc displays in minibuffer
                lsp-ui-sideline-enable nil)
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

  ;; Debug
  (use-package dap-mode
    :diminish
    :functions dap-hydra/nil
    :bind (:map lsp-mode-map
                ("<f5>" . dap-debug)
                ("M-<f5>" . dap-hydra))
    :hook ((after-init . dap-mode)
           (dap-mode . dap-ui-mode)
           (dap-session-created . (lambda (&_rest) (dap-hydra)))
           (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))

           (python-mode . (lambda () (require 'dap-python)))
           (ruby-mode . (lambda () (require 'dap-ruby)))
           (go-mode . (lambda () (require 'dap-go)))
           (java-mode . (lambda () (require 'dap-java)))
           ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
           (php-mode . (lambda () (require 'dap-php)))
           (elixir-mode . (lambda () (require 'dap-elixir)))
           ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))))

  ;; C/C++/Objective-C support
  (use-package ccls
    :defines projectile-project-root-files-top-down-recurring
    :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls)))
    :config
    (kevin/define-jump-handlers c-mode lsp-find-definition)
    (kevin/define-jump-handlers c++-mode lsp-find-definition)
    (with-eval-after-load 'projectile
      (setq projectile-project-root-files-top-down-recurring
            (append '("compile_commands.json" ".ccls")
                    projectile-project-root-files-top-down-recurring)))
    (setq ccls-executable "ccls"
          ccls-initialization-options `(:cache (:directory "/tmp/ccls-cache"),
                                               :compilationDatabaseDirectory "build")))
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
