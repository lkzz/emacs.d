;;; init-exec-from-path.el --- auto load system variables. -*- lexical-binding: t; -*-
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

(use-package exec-path-from-shell
  :defer t
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH" "GOROOT"))
  (setq exec-path-from-shell-arguments '("-l"))
  (add-hook 'after-init-hook 'exec-path-from-shell-initialize))

(provide 'init-exec-path)
;;; init-exec-path ends here
