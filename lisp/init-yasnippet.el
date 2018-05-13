;; init-yasnippet.el --- Initialize yasnippet configurations.
;;; Commentary:
;;; Code:

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :defer t
  :after (yasnippet)
  :ensure t)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
