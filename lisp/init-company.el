;;; init-company.el --- auto complate use company. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
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
  :defer 3
  :diminish company-mode "ⓒ"
  :bind (("M-/" . company-complete)
         ("C-c C-y" . company-yasnippet)
         :map company-active-map
         ("C-s" . company-filter-candidates)
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-g" . company-abort)
         ("<tab>" . company-complete-selection)
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
  (progn
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
    (setq company-frontends '(company-pseudo-tooltip-frontend
                              company-echo-metadata-frontend))
    (setq company-backends kevin/company-global-backends)))


;; doesn’t play well with company-childframe
(use-package company-quickhelp
  :defer t
  :if (display-graphic-p)
  :after company
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin))
  :hook (company-mode . company-quickhelp-mode)
  :init (company-quickhelp-mode 1)
  :config
  (progn
    (setq company-quickhelp-use-propertized-text t)
    (setq company-quickhelp-delay 0.6)
    (setq company-quickhelp-max-lines 30)))

;; (use-package company-childframe
;;   :diminish company-childframe-mode
;;   :after (company posframe)
;;   :config
;;   (progn
;;     (company-childframe-mode 1)))

;; ;; Show you likelier candidates at the top of the list
;; (use-package company-statistics
;;   :after company
;;   :hook (company-mode . company-statistics-mode)
;;   :config
;;   ;; save cache file to `user-cache-directory'
;;   (setq company-statistics-file (concat kevin/cache-directory
;;                                         "company-statistics-cache.el")))

;; (use-package company-box
;;   :after company
;;   :diminish company-box-mode
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   (setq company-box-backends-colors nil
;;         company-box-icons-elisp
;;         (list (all-the-icons-material "functions" :face 'all-the-icons-purple)
;;               (all-the-icons-material "check_circle" :face 'all-the-icons-blue)
;;               (all-the-icons-material "stars" :face 'all-the-icons-yellow)
;;               (all-the-icons-material "format_paint" :face 'all-the-icons-pink))
;;         company-box-icons-unknown (all-the-icons-material "find_in_page" :face 'all-the-icons-silver)
;;         company-box-icons-yasnippet (all-the-icons-material "short_text" :face 'all-the-icons-green)))

(provide 'init-company)
;;; init-company.el ends here
