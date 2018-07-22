;;; init-lua.el --- lua mode config. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:
                                        ;

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
                               (make-local-variable 'company-backends)
                               (add-to-list 'company-backends '(company-lua company-yasnippet))))
    ))

(provide 'init-lua)
;; init-lua.el ends here.
