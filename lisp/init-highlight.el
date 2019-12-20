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
  :straight (:type built-in)
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil))

;; Show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(use-package paren
  :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :config
  (set-face-foreground 'show-paren-match "red")      ;定义前景色
  (set-face-bold-p 'show-paren-match t)              ;加粗显示括号匹配
  (set-face-background 'show-paren-match nil)        ;定义背景色
  (set-face-underline 'show-paren-match t)           ;显示下划线
  (setq show-paren-delay 0.1
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Highlight show trailing whitespace
(use-package whitespace
  :straight (:type built-in)
  :diminish whitespace-mode "ⓦ"
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

(defun kevin/disable-highlight-indent-guides ()
  (when highlight-indent-guides-mode
    (highlight-indent-guides-mode -1)))

;; Highlight indent guide.
(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)
  ;; `highlight-indent-guides' breaks in these modes
  (add-hook 'visual-line-mode-hook #'kevin/disable-highlight-indent-guides)
  (add-hook 'org-indent-mode-hook #'kevin/disable-highlight-indent-guides)
  (setq highlight-indent-guides-delay 0.5
        highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray"))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((text-mode emacs-list-mode) . rainbow-mode))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 3))

;; Highlight TODO/FIXME/NOTE...
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       . ,(face-foreground 'warning))
          ("FIXME"      . ,(face-foreground 'error))
          ("HACK"       . ,(face-foreground 'font-lock-constant-face))
          ("REVIEW"     . ,(face-foreground 'font-lock-keyword-face))
          ("NOTE"       . ,(face-foreground 'success))
          ("DEPRECATED" . ,(face-foreground 'font-lock-doc-face)))))

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
  (kevin/set-leader-keys "ts" #'symbol-overlay-mode))

(provide 'init-highlight)
;;; init-highlight.el ends here
