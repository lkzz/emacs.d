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

;; Show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(use-package paren
  :ensure nil
  :defer t
  :init (show-paren-mode t))

;; Highlight matching paren
(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :hook (prog-mode . global-highlight-parentheses-mode))

;; Highlight show trailing whitespace
(use-package whitespace
  :ensure nil
  :defer t
  :diminish whitespace-mode
  :hook (after-init . whitespace-mode)
  :init
  (add-hook 'minibuffer-setup-hook (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (setq-default show-trailing-whitespace t)
  (setq whitespace-style '(face trailing))
  :config
  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)
    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
	  "Turn off whitespace mode before showing autocomplete box."
	  (if whitespace-mode
		  (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))
    (defadvice popup-delete (after my-restore-whitespace activate compile)
	  "Restore previous whitespace mode when deleting autocomplete box."
	  (if my-prev-whitespace-mode
		  (whitespace-mode 1)))))

;; An unobtrusive way to trim spaces from end of line
(use-package ws-butler
  :ensure t
  :defer t
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode)
  :init (setq ws-butler-keep-whitespace-before-point nil))

;; Highlight indent guide.
(use-package highlight-indent-guides
  :defer t
  :ensure t
  :diminish highlight-indent-guides-mode
  :if (display-graphic-p)
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-delay 0.5)
  (setq highlight-indent-guides-method 'character))

;; Colorize color names in buffers
(use-package rainbow-mode
  :defer t
  :ensure t
  :diminish rainbow-mode
  :hook ((text-mode . rainbow-mode)
         (prog-mode . rainbow-mode)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :disabled
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO/FIXME/BUG...
(use-package hl-todo
  :defer t
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces `(("TODO"  . ,(face-foreground 'warning))
                                ("FIXME" . ,(face-foreground 'error))
                                ("NOTE"  . ,(face-foreground 'success)))))

;; Show column indicator.
(use-package fill-column-indicator
  :disabled
  :ensure t
  :diminish auto-fill-mode
  :config
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
  (turn-on-auto-fill))

;; Beacon flashes the cursor whenever you adjust position.
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode t)
  (setq beacon-color "red")
  (setq beacon-size 80)
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode))

(use-package symbol-overlay
  :ensure t
  :defer t
  :diminish symbol-overlay-mode
  :bind (:map symbol-overlay-mode-map
              ("C-p" . symbol-overlay-jump-prev)
              ("C-n" . symbol-overlay-jump-next))
  :init
  (kevin/set-leader-keys "ts" 'symbol-overlay-mode))

(provide 'init-highlight)
;;; init-highlight.el ends here
