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
  :ensure nil
  :init
  (setq bookmark-default-file (concat kevin-cache-dir "bookmarks"))
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
  :commands restart-emacs
  :general (kevin/colon-key-define "e r" 'restart-emacs))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; History
(use-package saveplace
  :ensure nil
  :init (setq save-place-file (concat kevin-cache-dir "saveplace"))
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :hook ((after-init . recentf-mode)
         (kill-emacs-hook . recentf-cleanup))
  :init (setq recentf-max-saved-items 500
              recentf-save-file (concat kevin-cache-dir "recentf")
              recentf-auto-cleanup 'never
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
  (setq avy-background t)
  :general
  (kevin/space-key-define
    "j c" 'avy-goto-char-2
    "j f" 'beginning-of-defun
    "j l" 'avy-goto-line
    "j m" '(kevin/jump-match-delimiter :wk "goto-match-delimiter")
    "j w" 'avy-goto-word-or-subword-1))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode scala-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                                   'java-mode 'go-mode 'swift-mode)
                   (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                       (thing-at-point 'line))))))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq savehist-file (concat kevin-cache-dir "savehist")
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
  :general (hs-minor-mode-map "C-`" 'hs-toggle-hiding)
  :hook (prog-mode . hs-minor-mode))

;; Move to the beginning/end of line or code
(use-package mwim
  :general ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line
            [remap move-end-of-line] 'mwim-end-of-code-or-line))

(use-package helpful
  :general
  ("C-h f" 'helpful-callable
   "C-h v" 'helpful-variable
   "C-h k" 'helpful-key))

(use-package so-long
  :if is-emacs27-p
  :ensure nil
  :config
  (global-so-long-mode 1)
  (setq so-long-threshold 400)
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; Text files could possibly be too long too
  (add-to-list 'so-long-target-modes 'text-mode)
  ;; disable some mode that may be unnecessary/expensive for large buffer
  (add-to-list 'so-long-minor-modes 'rainbow-delimiters-mode)
  (add-to-list 'so-long-minor-modes 'rainbow-identifiers-mode)
  (add-to-list 'so-long-minor-modes 'rainbow-mode)
  (add-to-list 'so-long-minor-modes 'flycheck-mode)
  (add-to-list 'so-long-minor-modes 'eldoc-mode)
  (add-to-list 'so-long-minor-modes 'smartparens-mode)
  (add-to-list 'so-long-minor-modes 'highlight-numbers-mode)
  (add-to-list 'so-long-minor-modes 'ws-butler-mode)
  (add-to-list 'so-long-minor-modes 'undo-tree-mode)
  (add-to-list 'so-long-minor-modes 'highlight-indent-guide-mode)
  (add-to-list 'so-long-minor-modes 'hl-fill-column-mode))

(use-package transient
  :init
  (setq transient-levels-file (concat kevin-cache-dir "transient-levels.el")
        transient-values-file (concat kevin-cache-dir "transient-values.el")
        transient-history-file (concat kevin-cache-dir "transient/history.el")))

;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

(use-package direnv
  :hook
  (after-init . direnv-mode))

(provide 'init-misc)
;;; init-misc.el ends here
