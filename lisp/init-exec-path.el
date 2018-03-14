;;; init-exec-from-path.el --- auto load system variables
;;; Commentary:
;;; Code:

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
  	:ensure t
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

(provide 'init-exec-path)
;;; init-exec-path ends here
