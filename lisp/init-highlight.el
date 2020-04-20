;; init-highlight.el --- Initialize highlight configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2020  Kevin Leung
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
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

;; Show-paren-mode: subtle blinking of matching paren (defaults are ugly)
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (set-face-foreground 'show-paren-match "red")      ;定义前景色
  (set-face-bold-p 'show-paren-match t)              ;加粗显示括号匹配
  (set-face-background 'show-paren-match nil)        ;定义背景色
  (set-face-underline 'show-paren-match t)           ;显示下划线
  (setq show-paren-delay 0.1
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Highlight indent guide.
(use-package highlight-indent-guides
  :diminish highlight-indent-guides-mode
  :hook ((prog-mode conf-mode protobuf-mode) . highlight-indent-guides-mode)
  :config
  ;; (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)
  ;; `highlight-indent-guides' breaks in these modes
  ;; (add-hook 'visual-line-mode-hook #'kevin/disable-highlight-indent-guides)
  ;; (add-hook 'org-indent-mode-hook #'kevin/disable-highlight-indent-guides)
  (setq highlight-indent-guides-delay 0.5
        highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray"))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
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
  :general
  (kevin/space-key-define "t s" 'symbol-overlay-mode)
  (symbol-overlay-mode-map "M-p" 'symbol-overlay-jump-prev
                           "M-n" 'symbol-overlay-jump-next
                           "M-r" 'symbol-overlay-rename))

(provide 'init-highlight)
;;; init-highlight.el ends here
