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
        company-tooltip-flip-when-above nil
        company-echo-delay 0            ; remove annoying blinking
        company-idle-delay 0.0          ; set the completion menu pop-up delay
        company-minimum-prefix-length 1 ; pop up a completion menu by tapping a character
        company-show-numbers t          ; display numbers on the left
        company-tooltip-limit 10
        company-require-match nil
        company-selection-wrap-around t     ; make previous/next selection in the popup cycle
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
        company-backends '((company-capf :with company-yasnippet :with company-tabnine :separate)
                           (company-dabbrev company-keywords company-files)))
  :config
  (add-hook 'evil-normal-state-entry-hook (lambda ()
                                            (when company-candidates (company-abort))))
  ;; Remove duplicate candidate.
  (add-to-list 'company-transformers #'delete-dups)

  (defun my-lsp-fix-company-capf ()
    "Remove redundant `company-capf'."
    (setq company-backends
          (remove 'company-backends (remq 'company-capf company-backends))))
  (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

  ;; Uses machine learning to provide suggestions.
  (use-package company-tabnine
    :config
    (setq company-tabnine-max-num-results 3)
    ;; The free version of TabNine is good enough,
    ;; and below code is recommended that TabNine not always
    ;; prompt me to purchase a paid version in a large project.
    (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
      (let ((company-message-func (ad-get-arg 0)))
        (when (and company-message-func
                   (stringp (funcall company-message-func)))
          (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
            ad-do-it))))

    (defun company//sort-by-tabnine (candidates)
      "实现前2个候选项是company-capf的,接着的2个是TabNine的."
      (if (or (functionp company-backend)
              (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
          candidates
        (let ((candidates-table (make-hash-table :test #'equal))
              candidates-1
              candidates-2)
          (dolist (candidate candidates)
            (if (eq (get-text-property 0 'company-backend candidate)
                    'company-tabnine)
                (unless (gethash candidate candidates-table)
                  (push candidate candidates-2))
              (push candidate candidates-1)
              (puthash candidate t candidates-table)))
          (setq candidates-1 (nreverse candidates-1))
          (setq candidates-2 (nreverse candidates-2))
          (nconc (seq-take candidates-1 2)
                 (seq-take candidates-2 2)
                 (seq-drop candidates-1 2)
                 (seq-drop candidates-2 2)))))
    (add-to-list 'company-transformers 'company//sort-by-tabnine t))

  ;; Simple but effective sorting and filtering for Emacs.
  (use-package company-prescient
    :hook (company-mode . company-prescient-mode)
    :config (prescient-persist-mode +1))

  ;; This package requires emacs 26+, not compatible with emacs in a tty.
  (use-package company-box
    :diminish
    :hook (company-mode . company-box-mode)
    :init (setq company-box-backends-colors nil
                company-box-show-single-candidate t
                company-box-max-candidates 50
                company-box-highlight-prefix t
                company-box-doc-delay 0.3)
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
