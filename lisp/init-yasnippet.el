;; init-yasnippet.el --- Initialize yasnippet configurations.
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init (add-hook 'after-init-hook #'yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :defer t
  	:ensure t))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
