;; init-lua.el

(use-package lua-mode
  :defer t
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :init
  (progn
    (setq lua-indent-level 2)
    (setq lua-indent-string-contents t)))

(use-package company-lua
  :ensure t
  :after (lua-mode company)
  :config
  (progn
    (add-hook 'lua-mode-hook (lambda ()
                               (let ((local-lua-backends kevin/company-global-backends))
                                 (add-to-list 'local-lua-backends 'company-lua)
                                 (set (make-local-variable 'company-backends) local-lua-backends))
                               ))
    ))

(provide 'init-lua)
;; init-lua.el ends here.
