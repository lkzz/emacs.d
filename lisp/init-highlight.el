;; init-highlight.el --- Initialize highlight configurations. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Highlight matching paren
(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :hook (prog-mode . global-highlight-parentheses-mode))

;; Highlight indent guide.
(use-package highlight-indent-guides
  :defer t
  :ensure t
  :if (display-graphic-p)
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (progn
    (setq highlight-indent-guides-delay 0.5)
    (setq highlight-indent-guides-method 'character)))

;; Colorize color names in buffers
(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode
  :hook ((text-mode . rainbow-mode)
         (prog-mode . rainbow-mode)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO/FIXME/BUG...
(use-package hl-todo
  :defer t
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error))
          ("NOTE"  . ,(face-foreground 'success)))))

;; Show column indicator.
(use-package fill-column-indicator
  :ensure t
  :diminish auto-fill-mode
  :config
  (progn
    (kevin/set-leader-keys "tF" 'fci-mode)
    ;; NOTE fix display compatibility issue with company-mode
    (defun on-off-fci-before-company(command)
      (when (string= "show" command)
        (turn-off-fci-mode))
      (when (string= "hide" command)
        (turn-on-fci-mode)))
    (with-eval-after-load 'company-mode
      (advice-add 'company-call-frontends :before #'on-off-fci-before-company))
    (setq fci-rule-column 110)
    (setq fci-rule-width 1)
    (turn-on-auto-fill)))

(provide 'init-highlight)
;;; init-highlight.el ends here
