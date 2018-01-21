;;; init-projectile.el --- Initialize projectile
;;; Commentary:
;;; Code:

(use-package projectile
  :bind (("C-c p f" . 'projectile-find-file))
  :init (add-hook 'after-init-hook 'projectile-mode)
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t))

(provide 'init-projectile)
;;; init-projectile ends here
