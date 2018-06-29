(use-package doom-modeline
  ;; :ensure t
  ;; :defer t
  :load-path "vendor/doom-modeline/"
  :requires (shrink-path eldoc-eval)
  :hook (after-init . doom-modeline-init))

(provide 'init-doom-modeline)
