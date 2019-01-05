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
  :ensure t
  :diminish company-mode "ⓒ"
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("C-s" . company-filter-candidates)
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-g" . company-abort)
         ("C-/" . yas-expand-from-trigger-key)
         ("<tab>" . company-complete-common)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (add-hook 'company-completion-started-hook
            (lambda (&rest ignore)
              (when (and (bound-and-true-p evil-mode) (evil-insert-state-p))
                (define-key evil-insert-state-map (kbd "C-n") nil)
                (define-key evil-insert-state-map (kbd "C-p") nil))))
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 10)
  (setq company-require-match nil)
  (setq company-show-numbers t)
  ;; make previous/next selection in the popup cycles
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-global-modes '(not
                               comint-mode
                               erc-mode
                               message-mode
                               help-mode
                               gud-mode))
  (setq company-backends kevin/company-global-backends))

;; Show you likelier candidates at the top of the list
(use-package company-statistics
  :disabled
  :ensure t
  :after company
  :hook (company-mode . company-statistics-mode)
  :config
  (setq company-statistics-file (concat kevin-cache-directory
                                        "company-statistics-cache.el")))

;; This package requires emacs 26, not compatible with emacs in a tty.
(use-package company-box
  :ensure t
  :after company
  :diminish company-box-mode
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-enable-icon t)
  (setq company-box-doc-delay 0.5)
  (setq company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.8 :face 'all-the-icons-green)
        company-box-icons-unknown (all-the-icons-material "find_in_page" :height 0.8 :face 'all-the-icons-purple)
        company-box-icons-elisp
        (list (all-the-icons-material "functions"                  :height 0.8 :face 'all-the-icons-red)
              (all-the-icons-material "check_circle"               :height 0.8 :face 'all-the-icons-blue)
              (all-the-icons-material "stars"                      :height 0.8 :face 'all-the-icons-orange)
              (all-the-icons-material "format_paint"               :height 0.8 :face 'all-the-icons-pink))
        company-box-icons-lsp
        `((1  . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green)) ; text
          (2  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; method
          (3  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; function
          (4  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; constructor
          (5  . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))   ; field
          (6  . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))  ; variable
          (7  . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))   ; class
          (8  . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))   ; interface
          (9  . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))   ; module
          (10 . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))   ; property
          (11 . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))   ; unit
          (12 . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))   ; value
          (13 . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))   ; enum
          (14 . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))   ; keyword
          (15 . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))   ; snippet
          (16 . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))   ; color
          (17 . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))   ; file
          (18 . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))   ; reference
          (19 . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))   ; folder
          (20 . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))   ; enumMember
          (21 . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))   ; constant
          (22 . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))   ; struct
          (23 . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))   ; event
          (24 . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))   ; operator
          (25 . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red)))))

(use-package company-flx
  :ensure t
  :after company
  :config (company-flx-mode +1))

(provide 'init-company)
;;; init-company.el ends here
