;;; init-company.el --- auto complate use company. -*- lexical-binding: t; -*-
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
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t
        company-echo-delay 0            ; remove annoying blinking
        company-idle-delay 0.1          ; set the completion menu pop-up delay
        company-minimum-prefix-length 2 ; pop up a completion menu by tapping a character
        company-show-numbers t          ; display numbers on the left
        company-tooltip-limit 10
        company-require-match nil
        company-selection-wrap-around t     ; make previous/next selection in the popup cycle
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
        company-backends '(company-dabbrev-code ; 当前文件所属编程语言的语法关键词
                           company-keywords     ; 当前文件所属编程语言的语法关键词
                           company-dabbrev      ; 主要用来补全当前 buffer 中出现的 word
                           company-files)       ; 补全文件系统的路径后端
        company-frontends '(company-tng-frontend company-pseudo-tooltip-frontend company-echo-metadata-frontend))

  (with-eval-after-load 'yasnippet
    (defun company-backend-with-yas (backend)
      "Add `yasnippet' to company backend."
      (if (and (listp backend) (member 'company-yasnippet backend))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))

    (defun my-company-enbale-yas (&rest _)
      "Enable `yasnippet' in `company'."
      (setq company-backends (mapcar #'company-backend-with-yas company-backends)))
    ;; Enable in current backends
    (my-company-enbale-yas)
    ;; Enable in `lsp-mode'
    (advice-add #'lsp--auto-configure :after #'my-company-enbale-yas)

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

  ;; Better sorting and filtering
  (use-package company-prescient
    :init
    (setq prescient-save-file (concat kevin-cache-dir "prescient-save.el")))

  ;; This package requires emacs 26, not compatible with emacs in a tty.
  (use-package company-box
    :diminish
    :hook (company-mode . company-box-mode)
    :init (setq company-box-backends-colors nil
                company-box-show-single-candidate t
                company-box-max-candidates 50
                company-box-doc-delay 0.5)
    :config
    (defun company-remove-scrollbar-a (orig-fn &rest args)
      "This disables the company-box scrollbar, because:
https://github.com/sebastiencs/company-box/issues/44"
      :around #'company-box--update-scrollbar
      (cl-letf (((symbol-function #'display-buffer-in-side-window)
                 (symbol-function #'ignore)))
        (apply orig-fn args)))
    (advice-add #'company-box--update-scrollbar :around #'company-remove-scrollbar-a)

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
            company-box-icons-alist 'company-box-icons-all-the-icons)))

  )

(provide 'init-company)
;;; init-company.el ends here
