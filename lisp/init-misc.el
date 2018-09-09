;;; init-misc.el --- misc config files. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

;; bookmark 设置
(use-package bookmark
  :ensure nil
  :init
  (setq bookmark-default-file (concat kevin/cache-directory "bookmarks"))
  (kevin/declare-prefix "m" "bookmark")
  (kevin/set-leader-keys "ms" 'bookmark-set
                         "mr" 'bookmark-rename
                         "md" 'bookmark-delete
                         "mj" 'counsel-bookmark
                         "ml" 'bookmark-bmenu-list))

;; Elec pair
(use-package elec-pair
  :defer t
  :ensure nil
  :init (add-hook 'after-init-hook #'electric-pair-mode))

;; Hungry deletion
(use-package hungry-delete
  :defer t
  :diminish hungry-delete-mode "ⓗ"
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode))

(use-package restart-emacs
  :defer t)

(use-package server
  :defer t
  :init
  (server-mode 1)
  :config
  (unless (server-running-p)
    (server-start)))

;; History
(use-package saveplace
  :defer t
  :ensure nil
  :init
  (add-hook 'after-init-hook #'save-place-mode))

(use-package recentf
  :defer t
  :ensure nil
  :config
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                     (recentf-mode)
                                     (recentf-track-opened-file))))
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '((expand-file-name package-user-dir)
                          kevin/cache-directory
                          "bookmarks"
                          "COMMIT_EDITMSG\\'"
                          "custom.el")))


;; Delete selection if you insert
(use-package delsel
  :defer t
  :ensure t
  :init (add-hook 'after-init-hook #'delete-selection-mode))

;; Rectangle
(use-package rect
  :defer t
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))

;; Jump to things in Emacs tree-style
(use-package avy
  :defer t
  :ensure t
  :hook (after-init . avy-setup-default)
  :init
  (kevin/set-leader-keys
   "jc" 'avy-goto-char
   "jw" 'avy-goto-word-or-subword-1
   "jl" 'avy-goto-line
   "jp" #'kevin/goto-match-parent)
  :config (setq avy-background t))

;; Quickly follow links
(use-package ace-link
  :defer t
  :ensure t
  :bind (("M-o" . ace-link-addr))
  :init (add-hook 'after-init-hook #'ace-link-setup-default))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :defer t
  :ensure t
  :diminish aggressive-indent-mode
  :hook (prog-mode . global-aggressive-indent-mode)
  :config
  (progn
    (setq-default aggressive-indent-comments-too t)
    ;; NOTE: Disable in big files due to the performance issues
    ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
    (add-hook 'find-file-hook
              (lambda ()
                (if (> (buffer-size) (* 3000 80))
                    (aggressive-indent-mode -1))))
    ;; Disable in some modes
    (dolist (mode '(asm-mode web-mode html-mode css-mode robot-mode))
      (push mode aggressive-indent-excluded-modes))
    ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
    (add-to-list 'aggressive-indent-dont-indent-if
                 '(and (or (derived-mode-p 'c-mode)
                           (derived-mode-p 'c++-mode)
                           (derived-mode-p 'csharp-mode)
                           (derived-mode-p 'java-mode)
                           (derived-mode-p 'go-mode)
                           (derived-mode-p 'swift-mode))
                       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                           (thing-at-point 'line)))))))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :defer t
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :defer t
  :ensure t
  :diminish drag-stuff-mode
  :init (add-hook 'after-init-hook #'drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :defer t
  :ensure nil
  :init
  ;; show org ediffs unfolded
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all))
  ;; restore window layout when done
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Treat undo history as a tree
(use-package undo-tree
  :defer t
  :ensure t
  :diminish undo-tree-mode "ⓤ"
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat kevin/cache-directory "undo-tree-history"))))
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (global-undo-tree-mode))

(use-package savehist
  :defer t
  :ensure nil
  :init
  ;; Minibuffer history
  (setq savehist-file (concat kevin/cache-directory "savehist")
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
  :defer t
  :ensure nil
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding))
  :diminish hs-minor-mode)

;; Move to the beginning/end of line or code
(use-package mwim
  :defer t)

(use-package wgrep
  :defer t)

(use-package counsel-osx-app
  :defer t)

(use-package smex
  :defer t
  :ensure t
  :config
  (setq smex-save-file (concat kevin/cache-directory "smex-items"))
  (setq smex-history-length 10))

(provide 'init-misc)
;;; init-misc.el ends here
