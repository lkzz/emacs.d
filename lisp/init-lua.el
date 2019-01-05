;;; init-lua.el --- lua mode config. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package lua-mode
  :defer t
  :ensure t
  :mode ("\\.lua$" . lua-mode)
  :init
  (setq lua-indent-level 2)
  (setq lua-indent-string-contents t))

(use-package company-lua
  :ensure t
  :after (lua-mode company)
  :config
  (add-hook 'lua-mode-hook (lambda ()
                             (make-local-variable 'company-backends)
                             (setq company-backends kevin/company-global-backends)
                             (add-to-list 'company-backends 'company-lua))))

(provide 'init-lua)
;; init-lua.el ends here.
