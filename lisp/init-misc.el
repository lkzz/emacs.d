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
  (kevin/declare-prefix "m" "bookmark")
  (kevin/set-leader-keys
    "ms" 'bookmark-set
    "mr" 'bookmark-rename
    "md" 'bookmark-delete
    "mj" 'counsel-bookmark
    "ml" 'bookmark-bmenu-list))

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
  :init
  (kevin/set-leader-keys
    ";r" 'restart-emacs
    ";q"  'save-buffers-kill-terminal))

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
  :init (setq recentf-max-saved-items 300
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

;; Delete selection if you insert
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Rectangle
(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))

;; Jump to things in Emacs tree-style
(use-package avy
  :hook (after-init . avy-setup-default)
  :init
  (kevin/set-leader-keys
    "jc" 'avy-goto-char-2
    "jw" 'avy-goto-word-or-subword-1
    "jl" 'avy-goto-line)
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

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode "ⓤ"
  :commands (undo-tree-visualize)
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history nil
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-history-directory-alist `(("." . ,(concat kevin-cache-directory "undo-tree-history")))))

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

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))

(provide 'init-misc)
;;; init-misc.el ends here
