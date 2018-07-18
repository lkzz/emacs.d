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

(use-package indent-guide
  :defer t
  :ensure t
  :diminish indent-guide-global-mode "Ⓘ"
  :hook (prog-mode . indent-guide-global-mode)
  :config
  (progn
    ;; (setq indent-guide-recursive t)
    (setq indent-guide-char "|")
    (setq indent-guide-delay 0.3)))

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

(use-package fill-column-indicator
  :ensure t
  :defer t
  :diminish auto-fill-mode
  :commands (fci-mode)
  ;; :hook (prog-mode . fci-mode) ;; 导致company候选词偏移
  :init
  (progn
    (kevin/set-leader-keys "tF" 'fci-mode))
  :config
  (progn
    (setq fci-rule-column 80)
    (setq fci-rule-width 1)
    (turn-on-auto-fill)))

(provide 'init-highlight)
;;; init-highlight.el ends here
