;; init-highlight.el --- Initialize highlight configurations.
;;; Commentary:
;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :init (add-hook 'after-init-hook #'global-hl-line-mode))

;; Interactively highlight the current-window (by dimming the others)
(use-package dimmer
  :init (add-hook 'after-init-hook #'dimmer-mode))

;; Highlight symbols
(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ([C-f3] . symbol-overlay-put)
         ([f3] . symbol-overlay-jump-next)
         ([S-f3] . symbol-overlay-jump-prev)
         ([M-f3] . symbol-overlay-remove-all))
  :init (add-hook 'prog-mode-hook #'symbol-overlay-mode))

;; Highlight matching paren
(use-package paren
  :ensure nil
  :init (add-hook 'after-init-hook #'show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; Highlight surrounding parentheses
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))

;; Highlight indentions
(use-package highlight-indent-guides
  :init (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Highlight TODO/FIXME/BUG...
(use-package fic-mode
  :init (add-hook 'prog-mode-hook #'fic-mode)
  :config
  (setq fic-activated-faces '(font-lock-comment-face))
  (set-face-background 'fic-face "yellow")
  (set-face-background 'fic-author-face "yellow"))

;; Highlight uncommitted changes
(use-package diff-hl
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :init
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  :config
  (diff-hl-flydiff-mode 1)

  ;; Fall back to the display margin, if the fringe is unavailable
  (unless (display-graphic-p)
    (setq diff-hl-side 'right)
    (diff-hl-margin-mode 1))

  ;; Integration with magit and psvn
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (with-eval-after-load 'psvn
    (defadvice svn-status-update-modeline (after svn-update-diff-hl activate)
      (diff-hl-update))))

;; Highlight some operations
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :init (add-hook 'after-init-hook #'volatile-highlights-mode))



(provide 'init-highlight)

;;; init-highlight.el ends here
