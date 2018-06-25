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
  :defer t
  :diminish highlight-parentheses-mode
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

;; Highlight indentions
(use-package highlight-indent-guides
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))

;; Colorize color names in buffers
(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode
  :init
  (add-hook 'text-mode-hook #'rainbow-mode)
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :defer t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Highlight TODO/FIXME/BUG...
(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

;; (use-package fic-mode
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'prog-mode-hook #'fic-mode)
;;   :config
;;   (setq fic-activated-faces '(font-lock-comment-face))
;;   (set-face-background 'fic-face "DarkGoldenrod2")
;;   (set-face-background 'fic-author-face "DarkGoldenrod2"))

;; (use-package fill-column-indicator
;;   :ensure t
;;   :diminish auto-fill-mode
;;   :commands (fci-mode)
;;   :init (add-hook 'prog-mode-hook #'fci-mode)
;;   :config
;;   (progn
;;     (turn-on-auto-fill)))

(provide 'init-highlight)
;;; init-highlight.el ends here
