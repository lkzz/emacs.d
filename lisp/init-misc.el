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
  :defer t
  :ensure nil
  :config
  (setq save-place-file (concat kevin-cache-directory "saveplace"))
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-save-file (concat kevin-cache-directory "recentf")
        recentf-max-saved-items 200
        recentf-auto-cleanup 'never
        recentf-exclude '((expand-file-name package-user-dir)
                          kevin-cache-directory
                          "bookmarks"
                          "COMMIT_EDITMSG\\'"
                          "pyim"
                          (concat user-emacs-directory "straight")))
  (add-hook 'dired-mode-hook
            (lambda ()
              "Add dired directory to recentf file list."
              (recentf-add-file default-directory)))
  )

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
  :config (setq avy-background t))

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
  :disabled
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
  :config
  (setq savehist-file (concat kevin-cache-directory "savehist")
        savehist-save-minibuffer-history t
        savehist-autosave-interval nil ; save on kill only
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode +1)
  (add-hook 'kill-emacs-hook
            (defun kevin/unpropertize-kill-ring ()
              "Remove text properties from `kill-ring' for a smaller savehist file."
              (setq kill-ring (cl-loop for item in kill-ring
                                       if (stringp item)
                                       collect (substring-no-properties item)
                                       else if item collect it)))))

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
  :config
  (setq rg-group-result t
        rg-show-columns t)
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)
  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep 'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))
  (kevin/set-leader-keys
    "sc" 'rg-dwim-current-dir
    "sf" 'rg-dwim-current-file
    "s/" 'counsel-rg)
  (with-eval-after-load 'counsel
    (bind-keys :map rg-global-map
               ("c r" . counsel-rg)
               ("c s" . counsel-ag)
               ("c p" . counsel-pt)
               ("c f" . counsel-fzf))))

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


(use-package transient
  :init
  (setq transient-history-limit 50)
  (setq transient-save-history t)
  (setq transient-levels-file (concat kevin-cache-directory "transient/levels.el"))
  (setq transient-values-file (concat kevin-cache-directory "transient/values.el"))
  (setq transient-history-file (concat kevin-cache-directory "transient/history.el")))

(provide 'init-misc)
;;; init-misc.el ends here
