;;; init-company.el --- auto complate use company
;;; Commentary:
;;; Code:

(use-package company
  :defer t
  ;; :diminish company-mode "ⓐ"
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
              (when evil-mode
                (when (evil-insert-state-p)
                  (define-key evil-insert-state-map (kbd "C-n") nil)
                  (define-key evil-insert-state-map (kbd "C-p") nil)))))
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
                                 eshell-mode
                                 comint-mode
                                 erc-mode
                                 message-mode
                                 help-mode
                                 gud-mode))
    (setq company-frontends '(company-pseudo-tooltip-frontend
                              company-echo-metadata-frontend))
    (setq company-backends '(company-capf
                             company-dabbrev
                             company-ispell
                             company-yasnippet
                             company-keywords))
    ))

(use-package company-quickhelp
  :defer t
  :after company
  :hook (company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 0.6)
  (setq company-quickhelp-max-lines 30))

;; Show you likelier candidates at the top of the list
(use-package company-statistics
  :defer t
  :after company
  :hook (company-mode . company-statistics-mode)
  :config
  ;; save cache file to `user-cache-directory'
  (setq company-statistics-file (concat kevin/cache-directory
                                        "company-statistics-cache.el")))

(use-package company-shell
  :defer t
  :after company
  :config
  (add-to-list 'company-backends 'company-shell))

(autoload 'company-capf "company-capf")
(autoload 'company-dabbrev "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-elisp "company-elisp")
(autoload 'company-etags "company-etags")
(autoload 'company-files "company-files")
(autoload 'company-gtags "company-gtags")
(autoload 'company-ispell "company-ispell")
(autoload 'company-yasnippet "company-yasnippet")


(provide 'init-company)
;;; init-company.el ends here
