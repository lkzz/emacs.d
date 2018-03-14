;;; init-markdown.el --- custom markdown mode
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  ;; On the fly markdown preview
  (use-package flymd
    :ensure t
    :bind (:map markdown-mode-command-map
                ("f" . flymd-flyit))))

(provide 'init-markdown)
;;; init-markdown ends here
