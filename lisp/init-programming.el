(use-package elec-pair
  :ensure nil
  :init (add-hook 'after-init-hook #'electric-pair-mode))

(when kevin/is-mac
  (use-package dash-at-point
    :bind (("\C-cd" . dash-at-point)
           ("\C-ce" . dash-at-point-with-docset))))

(provide 'init-programming)
;;; init-programming.el ends here
