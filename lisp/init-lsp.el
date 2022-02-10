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

(use-package lsp-mode
  :diminish lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :general (lsp-mode-map "C-c C-d" 'lsp-describe-thing-at-point
                         "C-c C-n" 'lsp-rename
                         [remap xref-find-definitions] 'lsp-find-definition
                         [remap xref-find-references] 'lsp-find-references)
  :init
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance
  (setq read-process-output-max (* 1024 1024) ;; 1MB
        lsp-keymap-prefix "C-c l"
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
  (my-comma-leader-def
    "="  '(:ignore t :wk "formatting")
    "=b" 'lsp-format-buffer
    "=r" 'lsp-format-region
    "a"  '(:ignore t :wk "code")
    "aa" 'lsp-execute-code-action
    "ah" 'lsp-document-highlight
    "al" 'lsp-avy-lens
    "g"  '(:ignore t :wk "goto")
    "ga" 'xref-find-apropos
    "gd" 'lsp-find-definition
    "gD" 'lsp-find-declaration
    "ge" 'lsp-treemacs-errors-list
    "gh" 'lsp-treemacs-call-hierarchy
    "gi" 'lsp-find-implementation
    "gr" 'lsp-find-references
    "gt" 'lsp-find-type-definition
    "G"  '(:ignore t :wk "peek")
    "Gg" 'lsp-ui-peek-find-definitions
    "Gi" 'lsp-ui-peek-find-implementation
    "Gr" 'lsp-ui-peek-find-references
    "Gs" 'lsp-ui-peek-find-workspace-symbol
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
  (general-nmap lsp-mode-map
    "ga" 'xref-find-apropos
    "gd" 'lsp-find-definition
    "gD" 'lsp-find-declaration
    "ge" 'lsp-treemacs-errors-list
    "gh" 'lsp-treemacs-call-hierarchy
    "gi" 'lsp-find-implementation
    "gr" 'lsp-find-references
    "gt" 'lsp-find-type-definition)
  (with-no-warnings
    (defun my-lsp--init-if-visible (func &rest args)
      "Not enabling lsp in `git-timemachine-mode'."
      (unless (bound-and-true-p git-timemachine-mode)
        (apply func args)))
    (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)))

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :general (lsp-ui-mode-map [remap evil-goto-definition] 'lsp-ui-peek-find-definitions
                            [remap xref-find-definitions] 'lsp-ui-peek-find-definitions
                            [remap xref-find-references] 'lsp-ui-peek-find-references)
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-doc-enable (display-graphic-p)
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t)
        lsp-ui-doc-use-webkit nil

        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
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
              (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face nil t))))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

(use-package dap-mode
  :defines dap-python-executable
  :functions dap-hydra/nil
  :diminish
  :bind (:map lsp-mode-map
         ("<f5>" . dap-debug)
         ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-auto-configure-mode)
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (_args) (dap-hydra/nil)))
         (python-mode . (lambda () (require 'dap-python)))
         (ruby-mode . (lambda () (require 'dap-ruby)))
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (php-mode . (lambda () (require 'dap-php)))
         (elixir-mode . (lambda () (require 'dap-elixir)))
         ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
         (powershell-mode . (lambda () (require 'dap-pwsh))))
  :init
  (when (executable-find "python3")
    (setq dap-python-executable "python3")))

;; Ivy integration
(use-package lsp-ivy
  :after lsp-mode
  :general (lsp-mode-map [remap xref-find-apropos] 'lsp-ivy-workspace-symbol
                         "C-s-." 'lsp-ivy-global-workspace-symbol)
  :config
  (when (display-graphic-p)
    (defvar lsp-ivy-symbol-kind-icons
      `(,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.15) ; Unknown - 0
        ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.02) ; File - 1
        ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Module - 2
        ,(all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Namespace - 3
        ,(all-the-icons-octicon "package" :height 0.9 :v-adjust -0.15) ; Package - 4
        ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Class - 5
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Method - 6
        ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02) ; Property - 7
        ,(all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue) ; Field - 8
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-lpurple) ; Constructor - 9
        ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Enum - 10
        ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-lblue) ; Interface - 11
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Function - 12
        ,(all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue) ; Variable - 13
        ,(all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.02 :face 'all-the-icons-purple) ; Constant - 14
        ,(all-the-icons-faicon "text-width" :height 0.9 :v-adjust -0.02) ; String - 15
        ,(all-the-icons-material "format_list_numbered" :height 0.95 :v-adjust -0.15) ; Number - 16
        ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-lblue) ; Boolean - 17
        ,(all-the-icons-material "view_array" :height 0.95 :v-adjust -0.15) ; Array - 18
        ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0 :face 'all-the-icons-blue) ; Object - 19
        ,(all-the-icons-faicon "key" :height 0.9 :v-adjust -0.02) ; Key - 20
        ,(all-the-icons-octicon "tag" :height 0.9 :v-adjust 0.0) ; Null - 21
        ,(all-the-icons-material "format_align_right" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue) ; EnumMember - 22
        ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange) ; Struct - 23
        ,(all-the-icons-octicon "zap" :height 0.9 :v-adjust 0 :face 'all-the-icons-orange) ; Event - 24
        ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.15) ; Operator - 25
        ,(all-the-icons-faicon "arrows" :height 0.9 :v-adjust -0.02) ; TypeParameter - 26
        ))

    (lsp-defun my-lsp-ivy--format-symbol-match
      ((sym &as &SymbolInformation :kind :location (&Location :uri))
       project-root)
      "Convert the match returned by `lsp-mode` into a candidate string."
      (let* ((sanitized-kind (if (< kind (length lsp-ivy-symbol-kind-icons)) kind 0))
             (type (elt lsp-ivy-symbol-kind-icons sanitized-kind))
             (typestr (if lsp-ivy-show-symbol-kind (format "%s " type) ""))
             (pathstr (if lsp-ivy-show-symbol-filename
                          (propertize (format " · %s" (file-relative-name (lsp--uri-to-path uri) project-root))
                                      'face font-lock-comment-face)
                        "")))
        (concat typestr (lsp-render-symbol-information sym ".") pathstr)))
    (advice-add #'lsp-ivy--format-symbol-match :override #'my-lsp-ivy--format-symbol-match)))

;; Python: pyright
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :init
  (when (executable-find "python3")
    (setq lsp-pyright-python-executable-cmd "python3")))

(provide 'init-lsp)
;;; init-lsp.el ends here
