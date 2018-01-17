(add-hook 'after-init-hook 'recentf-mode)

(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:"))

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(provide 'init-recentf)
