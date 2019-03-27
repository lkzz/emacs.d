;;; init-misc.el --- misc config files. -*- lexical-binding: t; -*-
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

;; bookmark 设置
(use-package bookmark
  :ensure nil
  :init
  (setq bookmark-default-file (concat kevin-cache-directory "bookmarks"))
  (kevin/declare-prefix "m" "bookmark")
  (kevin/set-leader-keys "ms" 'bookmark-set
                         "mr" 'bookmark-rename
                         "md" 'bookmark-delete
                         "mj" 'counsel-bookmark
                         "ml" 'bookmark-bmenu-list))

;; Elec pair
(use-package elec-pair
  :ensure nil
  :init (add-hook 'after-init-hook #'electric-pair-mode))

;; Hungry deletion
(use-package hungry-delete
  :diminish hungry-delete-mode "ⓗ"
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode))

(use-package restart-emacs)

(use-package server
  :unless (bound-and-true-p server-mode)
  :init (server-mode 1)
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :config
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                    (recentf-mode)
                                    (recentf-track-opened-file))))
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          kevin-cache-directory
                          "bookmarks"
                          "COMMIT_EDITMSG\\'"
                          "pyim"
                          "elpa"
                          "vendor"
                          "custom.el")))


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
  (kevin/set-leader-keys "jc" 'avy-goto-char-2
                         "jw" 'avy-goto-word-or-subword-1
                         "jl" 'avy-goto-line)
  :config (setq avy-background t))

;; Quickly follow links
(use-package ace-link
  :bind (("M-o" . ace-link-addr))
  :hook (after-init . ace-link-setup-default))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook (prog-mode . global-aggressive-indent-mode)
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

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish drag-stuff-mode
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :init
  ;; show org ediffs unfolded
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all))
  ;; restore window layout when done
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode "ⓤ"
  :commands (undo-tree-visualize)
  :hook (after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history nil
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-history-directory-alist `(("." . ,(concat kevin-cache-directory "undo-tree-history")))))

(use-package savehist
  :ensure nil
  :init
  ;; Minibuffer history
  (setq savehist-file (concat kevin-cache-directory "savehist")
        enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (savehist-mode t))

;; Hideshow
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding))
  :hook (prog-mode . hs-minor-mode))

;; Move to the beginning/end of line or code
(use-package mwim)

(use-package smex
  :config
  (setq smex-history-length 10
        smex-save-file (concat kevin-cache-directory "smex-items")))

(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

(use-package ag
  :defines projectile-command-map
  :init
  (with-eval-after-load 'projectile
    (bind-key "s S" #'ag-project projectile-command-map))
  :config
  (setq ag-reuse-window t
        ag-reuse-buffers t
        ag-highlight-search t))

(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :config
  (setq rg-group-result t
        rg-show-columns t)
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)
  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep 'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))
  (when (fboundp 'ag)
    (bind-key "a" #'ag rg-global-map))
  (with-eval-after-load 'counsel
    (bind-keys :map rg-global-map
               ("c r" . counsel-rg)
               ("c s" . counsel-ag)
               ("c p" . counsel-pt)
               ("c f" . counsel-fzf))))

(provide 'init-misc)
;;; init-misc.el ends here
