(use-package corfu
  :straight (:host github :files (:defaults "extensions/*") :includes (corfu-indexed
                                                                       corfu-quick
                                                                       corfu-info
                                                                       corfu-history))
  :init
  (setq corfu-auto t
        corfu-cycle t
        corfu-count 15
        corfu-bar-width 0.5
        corfu-quit-at-boundary t
        corfu-quit-no-match t
        corfu-min-width 60
        corfu-max-width 100
        corfu-auto-delay 0.1
        corfu-auto-prefix 1)
  (global-corfu-mode)
  :bind (:map corfu-map
         ("C-n" . corfu-next)
         ("C-p" . corfu-previous)
         ("C-g" . corfu-quit)
         ("TAB" . corfu-next)
         ("S-TAB" . corfu-previous))
  :config
  (use-package corfu-doc
    :init
    (add-hook 'corfu-mode-hook #'corfu-doc-mode))

  ;; Optionally use the `orderless' completion style.
  (use-package orderless
    :init
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion))))))

  ;; completion backend
  (use-package cape
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-tex)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (setq cape-dabbrev-check-other-buffers nil)
    (add-to-list 'completion-at-point-functions #'cape-keyword)))

(provide 'init-corfu)
