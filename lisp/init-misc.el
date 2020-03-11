;;; init-misc.el --- misc config files. -*- lexical-binding: t; -*-
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

;; bookmark 设置
(use-package bookmark
  :defer t
  :ensure nil
  :init
  (setq bookmark-default-file (concat kevin-cache-directory "bookmarks"))
  :general
  (kevin/space-key-define
    "m" '(nil :which-key "Bookmark")
    "m s" 'bookmark-set
    "m r" 'bookmark-rename
    "m d" 'bookmark-delete
    "m j" 'counsel-bookmark
    "m l" 'bookmark-bmenu-list))

;; Elec pair
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

;; Hungry deletion
(use-package hungry-delete
  :diminish hungry-delete-mode "ⓗ"
  :hook (after-init . global-hungry-delete-mode))

(use-package restart-emacs
  :defer t
  :general
  (kevin/colon-key-define
    "e" '(nil :which-key "Emacs")
    "e r" 'restart-emacs
    "e q" 'save-buffers-kill-terminal
    "e i" '(kevin/open-init-file :wk "open-init-file")))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; History
(use-package saveplace
  :ensure nil
  :init (setq save-place-file (concat kevin-cache-directory "saveplace"))
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 10000
              recentf-exclude '("/tmp/"
                                "recentf$"
                                "\\.cask$"
                                "\\.mkv$"
                                "\\.mp[34]$"
                                "\\.avi$"
                                "\\.wav$"
                                "\\.pdf$"
                                "\\.docx?$"
                                "\\.xlsx?$"
                                "url"
                                "COMMIT_EDITMSG\\'"
                                "bookmarks"
                                "pyim"
                                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude))

;; Jump to things in Emacs tree-style
(use-package avy
  :hook (after-init . avy-setup-default)
  :init
  (setq avy-background t))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((lisp-mode lisp-interaction-mode emacs-lisp-mode clojure-mode) . aggressive-indent-mode)
  :config
  (setq-default aggressive-indent-comments-too t)
  ;; NOTE: Disable in big files due to the performance issues
  ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
  (add-hook 'find-file-hook
            (lambda ()
              (if (> (buffer-size) (* 3000 80))
                  (aggressive-indent-mode -1))))
  ;; Disable in some modes
  (dolist (mode '(go-mode asm-mode web-mode html-mode css-mode robot-mode))
    (push mode aggressive-indent-excluded-modes))
  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (or (derived-mode-p 'c-mode)
                         (derived-mode-p 'c++-mode)
                         (derived-mode-p 'csharp-mode)
                         (derived-mode-p 'java-mode)
                         ;; (derived-mode-p 'go-mode)
                         (derived-mode-p 'swift-mode))
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line))))))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq savehist-file (concat kevin-cache-directory "savehist")
        enable-recursive-minibuffers t
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 300))

;; Hideshow
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding))
  :hook (prog-mode . hs-minor-mode))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; An alternative M-x interface for Emacs
(use-package amx
  :hook (after-init . amx-mode)
  :init
  (kevin/space-key-define "SPC" 'amx)
  (setq amx-history-length 10
        amx-save-file (concat kevin-cache-directory "amx-items")))

(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
              ("c" . rg-dwim-current-dir)
              ("f" . rg-dwim-current-file)
              ("m" . rg-menu)
              :map rg-mode-map
              ("m" . rg-menu))
  :init
  (setq rg-show-columns t
        rg-group-result t))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key))
  :general
  (kevin/space-key-define
    "h" '(:ignore t :wk "help")
    "h f" 'helpful-callable
    "h v" 'helpful-variable
    "h k" 'helpful-key))

(use-package so-long
  :if is-emacs27-p
  :ensure nil
  :config (global-so-long-mode 1))

(provide 'init-misc)
;;; init-misc.el ends here
