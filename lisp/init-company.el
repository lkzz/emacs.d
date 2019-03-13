;;; init-company.el --- auto complate use company. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019  Kevin Leung
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

(defconst kevin/company-global-backends '(
                                          ;; 当前文件所属编程语言的语法关键词
                                          company-keywords
                                          ;; 使用 completion-at-point-functions 的后端
                                          company-capf
                                          ;; 主要用来补全当前 buffer 中出现的 word
                                          company-dabbrev
                                          ;; 使用 yasnippet 补全的后端
                                          company-yasnippet
                                          ;; 补全文件系统的路径后端
                                          company-files
                                          ))
(use-package company
  :diminish company-mode "ⓒ"
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("C-s" . company-filter-candidates)
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-g" . company-abort)
         ("C-/" . yas-expand-from-trigger-key)
         ("TAB" . company-complete-common)
         ("<tab>" . company-complete-common)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'company-completion-started-hook
            (lambda (&rest ignore)
              (when (and (bound-and-true-p evil-mode) (evil-insert-state-p))
                (define-key evil-insert-state-map (kbd "C-n") nil)
                (define-key evil-insert-state-map (kbd "C-p") nil))))
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t
        company-echo-delay 0                ; remove annoying blinking
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-require-match nil
        company-show-numbers t
        company-selection-wrap-around t     ; make previous/next selection in the popup cycle
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-transformers '(company-sort-by-occurrence)
        company-global-modes '(not
                               comint-mode
                               erc-mode
                               message-mode
                               help-mode
                               gud-mode)
        company-backends kevin/company-global-backends))

;; Show you likelier candidates at the top of the list
(use-package company-statistics
  :disabled
  :after company
  :hook (company-mode . company-statistics-mode)
  :config
  (setq company-statistics-file (concat kevin-cache-directory
                                        "company-statistics-cache.el")))

;; This package requires emacs 26, not compatible with emacs in a tty.
(use-package company-box
  :disabled
  :after company
  :requires all-the-icons
  :diminish company-box-mode
  :hook (company-mode . company-box-mode)
  :load-path "vendor/company-box-20180607.1545"
  :preface
  ;; refer: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-company.el
  (defun kevin/company-box-icon (family icon icon_face &rest args)
    "Defines icons using `all-the-icons' for `company-box'."
    (when icon
      (let ((icon (pcase family
                    ('octicon (all-the-icons-octicon icon :v-adjust -0.05 :face icon_face args))
                    ('faicon (all-the-icons-faicon icon :v-adjust -0.0575 :face icon_face))
                    ('material (all-the-icons-material icon :v-adjust -0.225 :face icon_face args))
                    ('alltheicon (all-the-icons-alltheicon icon :face icon_face args)))))
        (unless (symbolp icon)
          (concat icon
                  (propertize " " 'face 'variable-pitch))))))
  :init
  (setq company-box-enable-icon (display-graphic-p))
  :config
  (setq company-box-enable-icon t
        company-box-doc-delay 0.5
        company-box-backends-colors nil
        company-box-max-candidates 50)

  (setq company-box-icons-yasnippet
        (kevin/company-box-icon 'octicon "file-code" 'all-the-icons-green))

  (setq company-box-icons-unknown
        (kevin/company-box-icon 'octicon "file-text" 'all-the-icons-purple))

  (setq company-box-icons-elisp
        (list
         (kevin/company-box-icon 'material "functions" 'all-the-icons-red)        ; Function
         (kevin/company-box-icon 'faicon "tag" 'all-the-icons-blue)               ; Variable
         (kevin/company-box-icon 'faicon "cog"  'all-the-icons-orange)            ; Feature
         (kevin/company-box-icon 'material "palette" 'all-the-icons-pink)         ; Face
         ))

  (setq company-box-icons-lsp
        `(( 1  . ,(kevin/company-box-icon 'material "text_fields" 'all-the-icons-purple))         ; Text
          ( 2  . ,(kevin/company-box-icon 'material "functions" 'all-the-icons-red))              ; Method
          ( 3  . ,(kevin/company-box-icon 'material "functions" 'all-the-icons-red))              ; Function
          ( 4  . ,(kevin/company-box-icon 'material "functions" 'all-the-icons-red))              ; Constructor
          ( 5  . ,(kevin/company-box-icon 'faicon "tag"  'all-the-icons-blue))                    ; Field
          ( 6  . ,(kevin/company-box-icon 'faicon "tag"  'all-the-icons-blue))                    ; Variable
          ( 7  . ,(kevin/company-box-icon 'material "class"  'all-the-icons-orange))              ; Class
          ( 8  . ,(kevin/company-box-icon 'faicon "cogs" 'all-the-icons-orange))                  ; Interface
          ( 9  . ,(kevin/company-box-icon 'alltheicon "less" 'all-the-icons-yellow))              ; Module
          (10  . ,(kevin/company-box-icon 'faicon "wrench" 'all-the-icons-yellow))                ; Property
          (11  . ,(kevin/company-box-icon 'faicon "tag"  'all-the-icons-blue))                    ; Unit
          (12  . ,(kevin/company-box-icon 'faicon "tag"  'all-the-icons-blue))                    ; Value
          (13  . ,(kevin/company-box-icon 'faicon "file-text-o" 'all-the-icons-purple))           ; Enum
          (14  . ,(kevin/company-box-icon 'material "format_align_center" 'all-the-icons-yellow)) ; Keyword
          (15  . ,(kevin/company-box-icon 'material "content_paste" 'all-the-icons-yellow))       ; Snippet
          (16  . ,(kevin/company-box-icon 'material "palette" 'all-the-icons-yellow))             ; Color
          (17  . ,(kevin/company-box-icon 'faicon "file" 'all-the-icons-yellow))                  ; File
          (18  . ,(kevin/company-box-icon 'faicon "tag"  'all-the-icons-blue))                    ; Reference
          (19  . ,(kevin/company-box-icon 'faicon "folder" 'all-the-icons-yellow))                ; Folder
          (20  . ,(kevin/company-box-icon 'faicon "tag"  'all-the-icons-blue))                    ; EnumMember
          (21  . ,(kevin/company-box-icon 'faicon "tag"  'all-the-icons-blue))                    ; Constant
          (22  . ,(kevin/company-box-icon 'faicon "cog" 'all-the-icons-orange))                   ; Struct
          (23  . ,(kevin/company-box-icon 'faicon "bolt" 'all-the-icons-yellow))                  ; Event
          (24  . ,(kevin/company-box-icon 'faicon "tag"  'all-the-icons-blue))                    ; Operator
          (25  . ,(kevin/company-box-icon 'faicon "cog" 'all-the-icons-orange))                   ; TypeParameter
          ))
  )

(provide 'init-company)
;;; init-company.el ends here
