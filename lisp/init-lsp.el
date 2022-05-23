;; init-lsp.el --- Initialize lsp (Language Server Protocol) configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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

(pcase my-lsp-backend
  ('lsp-bridge
   (use-package lsp-bridge
     :straight (:host github :repo "manateelazycat/lsp-bridge" :files (:defaults "*"))
     :init
     (when is-mac-p
       (setq lsp-bridge-python-command "/usr/local/bin/python3"))
     (dolist (hook (list
                    'c-mode-hook
                    'c++-mode-hook
                    'java-mode-hook
                    'python-mode-hook
                    'ruby-mode-hook
                    'rust-mode-hook
                    'elixir-mode-hook
                    'go-mode-hook
                    'haskell-mode-hook
                    'haskell-literate-mode-hook
                    'dart-mode-hook
                    'scala-mode-hook
                    'typescript-mode-hook
                    'typescript-tsx-mode-hook
                    'js2-mode-hook
                    'js-mode-hook
                    'rjsx-mode-hook
                    'tuareg-mode-hook
                    'latex-mode-hook
                    'Tex-latex-mode-hook
                    'texmode-hook
                    'context-mode-hook
                    'texinfo-mode-hook
                    'bibtex-mode-hook
                    'clojure-mode-hook
                    'clojurec-mode-hook
                    'clojurescript-mode-hook
                    'clojurex-mode-hook
                    'sh-mode-hook
                    'web-mode-hook))
       (add-hook hook (lambda ()
                        (setq-local corfu-auto nil) ;; let lsp-bridge control when popup completion frame
                        (lsp-bridge-mode 1))))
     :config
     (setq lsp-bridge-completion-provider 'corfu)
     ;; (setq lsp-bridge-enable-log t)
     (require 'lsp-bridge-icon)        ;; show icons for completion items, optional
     (require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
     ;; For Xref support
     (add-hook 'lsp-bridge-mode-hook (lambda ()
                                       (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t)))
     (general-evil-define-key 'normal lsp-bridge-mode-map
       "ga" 'xref-find-apropos
       "gd" 'lsp-bridge-find-def
       "K"  'lsp-bridge-lookup-documentation
       "gi" 'lsp-bridge-find-impl
       "gr" 'lsp-bridge-find-references)))
  ('eglot
   (use-package eglot
     :hook ((python-mode c-mode c++-mode go-mode) . eglot-ensure)
     :config
     (setq eglot-sync-connect 1
           eglot-connect-timeout 10
           eglot-autoshutdown t
           eglot-send-changes-idle-time 0.5
           eglot-events-buffer-size 0
           ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
           ;;      its popup rule causes eglot to steal focus too often.
           eglot-auto-display-help-buffer nil
           eldoc-echo-area-use-multiline-p nil
           eglot-stay-out-of '(flymake))
     ;; (evil-define-key 'normal 'global "K" 'my/eglot-help-at-point)
     (general-evil-define-key 'normal lsp-bridge-mode-map
       "ga" 'xref-find-apropos
       "gd" 'eglot-find-declaration
       "K"  'lsp-bridge-lookup-documentation
       "gi" 'eglot-find-implementation
       "gr" 'eglot-find-typeDefinition)
     (my/global-leader-define :keymaps 'override
       "ca" '(eglot-code-actions :wk "Code Action")
       "cr" '(eglot-rename :wk "Rename symbol")
       "cI" '(eglot-code-action-organize-imports :wk "Organize import")
       "cs" '(consult-eglot-symbols :wk "Symbols in project")
       "cd" '(eglot-find-declaration :wk "Jump to definition")
       "ci" '(eglot-find-implementation :wk "Find implementation")
       "cD" '(eglot-find-typeDefinition :wk "Find type definition"))))
  ('lsp-mode
   (use-package lsp-mode
     :diminish lsp-mode
     :hook (lsp-mode . lsp-enable-which-key-integration)
     :bind(:map lisp-mode-map
                ("C-c C-d" . lsp-describe-thing-at-point)
                ("C-c C-n" . lsp-rename)
                ([remap xref-find-definitions] . lsp-find-definition)
                ([remap xref-find-references] . lsp-find-references))
     :init
     (dolist (hook (list
                    'c-mode-hook
                    'c++-mode-hook
                    'python-mode-hook
                    'rust-mode-hook
                    'go-mode-hook))
       (add-hook hook 'lsp-deferred))
     ;; Performace tuning
     ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
     (setq read-process-output-max (* 1024 1024)) ;; 1MB
     (setenv "LSP_USE_PLISTS" "true")
     (setq lsp-keymap-prefix "C-c l"
           lsp-keep-workspace-alive nil
           lsp-response-timeout 5

           lsp-go-links-in-hover nil

           lsp-headerline-breadcrumb-enable nil

           lsp-eldoc-render-all nil
           lsp-eldoc-enable-hover nil

           lsp-signature-auto-activate nil
           lsp-signature-render-documentation nil

           lsp-modeline-diagnostics-enable nil
           lsp-modeline-code-actions-enable nil
           lsp-modeline-workspace-status-enable nil

           lsp-enable-folding nil
           lsp-enable-indentation nil
           lsp-enable-file-watchers nil
           lsp-enable-on-type-formatting nil
           lsp-enable-text-document-color nil
           lsp-enable-symbol-highlighting nil)

     ;; For `lsp-clients'
     (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
     :config
     (general-evil-define-key 'normal lsp-mode-map
       "ga" 'xref-find-apropos
       "gd" 'lsp-find-definition
       "gD" 'lsp-find-declaration
       "ge" 'lsp-treemacs-errors-list
       "gh" 'lsp-treemacs-call-hierarchy
       "gi" 'lsp-find-implementation
       "gr" 'lsp-find-references
       "gt" 'lsp-find-type-definition)
     (my/local-leader-define
       "="  '(:ignore t :wk "formatting")
       "=b" 'lsp-format-buffer
       "=r" 'lsp-format-region
       "c"  '(:ignore t :wk "code")
       "ca" 'lsp-execute-code-action
       "ch" 'lsp-document-highlight
       "cl" 'lsp-avy-lens
       "g"  '(:ignore t :wk "goto")
       "ga" 'xref-find-apropos
       "gd" 'lsp-find-definition
       "gD" 'lsp-find-declaration
       "ge" 'lsp-treemacs-errors-list
       "gh" 'lsp-treemacs-call-hierarchy
       "gi" 'lsp-find-implementation
       "gr" 'lsp-find-references
       "gt" 'lsp-find-type-definition
       "p"  '(:ignore t :wk "peek")
       "pg" 'lsp-ui-peek-find-definitions
       "pi" 'lsp-ui-peek-find-implementation
       "pr" 'lsp-ui-peek-find-references
       "ps" 'lsp-ui-peek-find-workspace-symbol
       "h"  '(:ignore t :wk "help")
       "hg" 'lsp-ui-doc-glance
       "hh" 'lsp-describe-thing-at-point
       "hs" 'lsp-signature-activate
       "i"  '(:ignore t :wk "import")
       "r"  '(:ignore t :wk "refactor")
       "rr" 'lsp-rename
       "t"  '(:ignore t :wk "toggle")
       "w"  '(:ignore t :wk "workspace")
       "wa" 'lsp-workspace-folders-add
       "wd" 'lsp-describe-session
       "wq" 'lsp-workspace-shutdown
       "wr" 'lsp-workspace-restart)

     (defun my/lsp--init-if-visible (func &rest args)
       "Not enabling lsp in `git-timemachine-mode'."
       (unless (bound-and-true-p git-timemachine-mode)
         (apply func args)))
     (advice-add #'lsp--init-if-visible :around #'my/lsp--init-if-visible)

     (use-package lsp-ui
       :custom-face
       (lsp-ui-sideline-code-action ((t (:inherit warning))))
       :bind (:map lsp-ui-mode-map
                   ([remap evil-goto-definition] . lsp-ui-peek-find-definitions)
                   ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                   ([remap xref-find-references] . lsp-ui-peek-find-references))
       :hook (lsp-mode . lsp-ui-mode)
       :init
       (setq lsp-ui-doc-enable (display-graphic-p)
             lsp-ui-doc-delay 0.5
             lsp-ui-doc-include-signature nil
             lsp-ui-doc-position 'at-point
             lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t)
             lsp-ui-doc-use-webkit nil

             lsp-ui-sideline-show-hover nil
             lsp-ui-sideline-show-diagnostics nil
             lsp-ui-sideline-ignore-duplicate t
             lsp-ui-sideline-show-code-actions nil

             lsp-ui-imenu-enable t
             lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                   ,(face-foreground 'font-lock-string-face)
                                   ,(face-foreground 'font-lock-constant-face)
                                   ,(face-foreground 'font-lock-variable-name-face)))
       :config
       ;; HACK: lsp-ui-doc frame background color when use doom-one theme
       (add-to-list 'lsp-ui-doc-frame-parameters '(background-color . "#2e3138"))
       ;; (add-to-list 'lsp-ui-doc-frame-parameters '(background-color . "#313131"))
       ;; Reset `lsp-ui-doc' after loading theme
       (add-hook 'after-load-theme-hook
                 (lambda ()
                   (setq lsp-ui-doc-border (face-background 'posframe-border nil t))))

       ;; `C-g'to close doc
       (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

     ;; Python: pyright
     (use-package lsp-pyright
       :hook (python-mode . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred)))
       :init
       (when (executable-find "python3")
         (setq lsp-pyright-python-executable-cmd "python3"))))))

(provide 'init-lsp)
;;; init-lsp.el ends here
