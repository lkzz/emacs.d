;;; init-company.el --- auto complate use company
;;; Commentary:
;;; Code:

(use-package company
  :ensure t
  :defer t
  :diminish company-mode "ⓐ"
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
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 2)
    (setq company-tooltip-limit 10)
    (setq company-require-match nil)
    (setq company-show-numbers t)
    ;; make previous/next selection in the popup cycles
    (setq company-selection-wrap-around t)
    (setq company-dabbrev-ignore-case t)
    (setq company-dabbrev-downcase nil)

    ;; Support yas in commpany
    ;; Note: Must be the last to involve all backends
    (defvar company-enable-yas t
      "Enable yasnippet for all backends.")

    (defun company-backend-with-yas (backend)
      (if (or (not company-enable-yas)
              (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    (setq company-backends (mapcar #'company-backend-with-yas company-backends))))

(use-package company-quickhelp
  :ensure t
  :defer t
  :after company
  :hook (company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-use-propertized-text t)
  (setq company-quickhelp-delay 0.6)
  (setq company-quickhelp-max-lines 30))

;; Show you likelier candidates at the top of the list
(use-package company-statistics
  :ensure t
  :defer t
  :after company
  :hook (company-mode . company-statistics-mode)
  :config
  ;; save cache file to `user-cache-directory'
  (setq company-statistics-file (concat kevin/cache-directory
                                        "company-statistics-cache.el")))

(use-package company-shell
  :ensure t
  :defer t
  :after company
  :config
  (add-to-list 'company-backends 'company-shell))

(provide 'init-company)
;;; init-company.el ends here
