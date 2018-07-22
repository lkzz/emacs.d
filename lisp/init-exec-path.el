;;; init-exec-from-path.el --- auto load system variables. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :defer t
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH" "GOROOT" "GO15VENDOREXPERIMENT"))
    (setq exec-path-from-shell-arguments '("-l"))
    (add-hook 'after-init-hook 'exec-path-from-shell-initialize)))

(provide 'init-exec-path)
;;; init-exec-path ends here
