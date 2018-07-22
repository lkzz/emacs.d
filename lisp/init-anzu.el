;;; init-anzu.el --- Show number of matches in mode-line while searching
;;; Commentary:
;;; Code:

(use-package anzu
  :defer t
  :ensure t
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config (setq anzu-replace-to-string-separator
                (if (char-displayable-p ?→) " → " " -> ")))

(provide 'init-anzu)
;;; init-anzu ends here
