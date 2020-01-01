;;; init-lua.el --- lua mode config. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2020  Kevin Leung
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
  :mode ("\\.lua$" . lua-mode)
  :init
  (setq lua-indent-level 2)
  (setq lua-indent-string-contents t)
  (kevin/define-jump-handlers lua-mode))

(use-package company-lua
  :after (lua-mode company)
  :config
  (kevin/add-company-backend :backend company-lua :mode lua-mode))

(provide 'init-lua)
;; init-lua.el ends here.
