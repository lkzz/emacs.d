;;; init-projectile.el --- Initialize projectile
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :diminish projectile-mode " ⓟ"
  :bind (("C-c p f" . 'projectile-find-file))
  :init (add-hook 'after-init-hook 'projectile-mode)
  :config
  (setq projectile-known-projects-file
        (concat kevin/cache-directory "projectile-bookmarks.eld"))
  (setq projectile-cache-file
        (concat kevin/cache-directory "projectile.cache"))
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t))

(provide 'init-projectile)
;;; init-projectile ends here
