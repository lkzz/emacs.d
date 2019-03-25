;; init-highlight.el --- Initialize highlight configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom-face
  (show-paren-match ((t (:background "SkyBlue2" :foreground "red" :underline t))))
  :config
  (setq show-paren-delay 0
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Highlight matching paren
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :hook (prog-mode . global-highlight-parentheses-mode))

;; Highlight show trailing whitespace
(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :preface
  (defun kevin/turn-off-whitespace-highlight ()
    (setq show-trailing-whitespace nil))
  :hook (((prog-mode outline-mode conf-mode) . whitespace-mode)
         ((minibuffer-setup eshell-mode) . kevin/turn-off-whitespace-highlight))
  :init
  (setq show-trailing-whitespace t
        whitespace-style '(face trailing))
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
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode)
  :init (setq ws-butler-keep-whitespace-before-point nil))

;; Highlight indent guide.
(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :if (display-graphic-p)
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-delay 0.5
        highlight-indent-guides-method 'character))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((text-mode . rainbow-mode)
         (prog-mode . rainbow-mode)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO/FIXME/BUG...
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces `(("TODO"  . ,(face-foreground 'warning))
                                ("FIXME" . ,(face-foreground 'error))
                                ("NOTE"  . ,(face-foreground 'success)))))

;; Show column indicator.
(use-package fill-column-indicator
  :disabled
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
  :if (display-graphic-p)
  :diminish beacon-mode
  :config
  (beacon-mode t)
  (setq beacon-size 40
        beacon-color "red")
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode))

(use-package symbol-overlay
  :diminish symbol-overlay-mode "ⓢ"
  :bind (:map symbol-overlay-mode-map
              ("M-p" . symbol-overlay-jump-prev)
              ("M-n" . symbol-overlay-jump-next)
              ("M-r" . symbol-overlay-rename))
  :init
  (kevin/set-leader-keys "ts" 'symbol-overlay-mode))

(provide 'init-highlight)
;;; init-highlight.el ends here
