;;; init-programming.el --- common config for prog mode
;;; Commentary:
;;; Code:

(use-package elec-pair
  :ensure nil
  :init (add-hook 'after-init-hook #'electric-pair-mode))

(when #'kevin/is-mac
  (use-package dash-at-point
    :bind (("\C-cD" . dash-at-point)
       ("\C-ce" . dash-at-point-with-docset))))

(use-package toml-mode
  :init
  :after go-mode)

(add-hook 'after-save-hook 'kevin/revert-buffer-no-confirm)

(provide 'init-programming)
;;; init-programming.el ends here
