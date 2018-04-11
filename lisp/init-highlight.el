;; init-highlight.el --- Initialize highlight configurations.
;;; Commentary:
;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :defer t
  :init (add-hook 'after-init-hook #'global-hl-line-mode))

;; Highlight matching paren
(use-package paren
  :ensure nil
  :defer t
  :init (add-hook 'after-init-hook #'show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; Highlight surrounding parentheses
(use-package highlight-parentheses
  :ensure t
  :defer t
  :diminish highlight-parentheses-mode
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

;; Highlight indentions
(use-package highlight-indent-guides
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))

;; Colorize color names in buffers
(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish rainbow-mode
  :init
  (add-hook 'text-mode-hook #'rainbow-mode)
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Highlight TODO/FIXME/BUG...
(use-package fic-mode
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'fic-mode)
  :config
  (setq fic-activated-faces '(font-lock-comment-face))
  (set-face-background 'fic-face "DarkGoldenrod2")
  (set-face-background 'fic-author-face "DarkGoldenrod2"))

;; Highlight uncommitted changes
(use-package diff-hl
  :ensure t
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :init
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (custom-set-faces
   '(diff-hl-insert ((t (:background "#7ccd7c"))))
   '(diff-hl-change ((t (:background "#3a81c3"))))
   '(diff-hl-delete ((t (:background "#ee6363")))))
  :config
  (diff-hl-flydiff-mode 1))

  (provide 'init-highlight)
;;; init-highlight.el ends here
