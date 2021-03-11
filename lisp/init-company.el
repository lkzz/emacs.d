;;; init-company.el --- auto complate use company. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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

(use-package company
  :diminish company-mode "ⓒ"
  :general
  (company-active-map "C-s" 'company-filter-candidates
                      "C-p" 'company-select-previous
                      "C-n" 'company-select-next
                      "C-u" 'company-previous-page
                      "C-d" 'company-next-page
                      "<tab>" 'company-complete-common-or-cycle)
  (company-search-map "C-p" 'company-select-previous
                      "C-n" 'company-select-next)
  :hook (after-init . global-company-mode)
  :init
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t
        company-echo-delay 0            ; remove annoying blinking
        company-idle-delay 0.0          ; set the completion menu pop-up delay
        company-minimum-prefix-length 2 ; pop up a completion menu by tapping a character
        company-show-numbers t          ; display numbers on the left
        company-tooltip-limit 10
        company-require-match nil
        company-selection-wrap-around t     ; make previous/next selection in the popup cycle
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
        company-backends '((company-capf :with company-yasnippet :with company-tabnine :separate)
                           (company-dabbrev-code company-keywords company-files)
                           company-dabbrev))
  (add-hook 'evil-normal-state-entry-hook (lambda ()
                                            (when company-candidates (company-abort))))
  :config
  (with-eval-after-load 'yasnippet
    (defun my-lsp-fix-company-capf ()
      "Remove redundant `company-capf'."
      (setq company-backends
            (remove 'company-backends (remq 'company-capf company-backends))))
    (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

    (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
      "Enable yasnippet but disable it inline."
      (if (eq command 'prefix)
          (when-let ((prefix (funcall fun 'prefix)))
            (unless (memq (char-before (- (point) (length prefix)))
                          '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
              prefix))
        (progn
          (when (and (bound-and-true-p lsp-mode)
                     arg (not (get-text-property 0 'yas-annotation-patch arg)))
            (let* ((name (get-text-property 0 'yas-annotation arg))
                   (snip (format "%s (Snippet)" name))
                   (len (length arg)))
              (put-text-property 0 len 'yas-annotation snip arg)
              (put-text-property 0 len 'yas-annotation-patch t arg)))
          (funcall fun command arg))))
    (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline))

  (use-package company-tabnine)

  ;; This package requires emacs 26+, not compatible with emacs in a tty.
  (use-package company-box
    :diminish
    :hook (company-mode . company-box-mode)
    :init (setq company-box-backends-colors nil
                company-box-show-single-candidate t
                company-box-max-candidates 50
                company-box-highlight-prefix t
                company-box-doc-delay 0.5)
    :config
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

    (when (and (display-graphic-p) (require 'all-the-icons nil t))
      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (declare-function all-the-icons-octicon 'all-the-icons)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
              (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
              (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
              (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
              (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
              (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
              (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
              (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
              (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
              (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
              (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
              (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
              (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
              (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
              (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
              (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
            company-box-icons-alist 'company-box-icons-all-the-icons))))

(provide 'init-company)
;;; init-company.el ends here
