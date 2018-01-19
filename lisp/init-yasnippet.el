;; init-yasnippet.el --- Initialize yasnippet configurations.
;;; Commentary:
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :init (add-hook 'after-init-hook #'yas-global-mode)
  :config (use-package yasnippet-snippets))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
