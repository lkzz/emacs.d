(autoload 'mwim "mwim" nil t)
(autoload 'mwim-beginning "mwim" nil t)
(autoload 'mwim-end "mwim" nil t)
(autoload 'mwim-beginning-of-code-or-line "mwim" nil t)
(autoload 'mwim-beginning-of-line-or-code "mwim" nil t)
(autoload 'mwim-beginning-of-code-or-line-or-comment "mwim" nil t)
(autoload 'mwim-end-of-code-or-line "mwim" nil t)
(autoload 'mwim-end-of-line-or-code "mwim" nil t)


(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
(global-set-key (kbd "<home>") 'mwim-beginning-of-line-or-code)
(global-set-key (kbd "<end>") 'mwim-end-of-line-or-code)


(provide 'init-mwim)
