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
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :hook ((after-init . recentf-mode)
         (kill-emacs-hook . recentf-cleanup))
  :init (setq recentf-max-saved-items 500
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
                                (concat "^" kevin-cache-dir ".+$")))
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

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq history-length 1000
        savehist-autosave-interval 300
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)))

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

;; Writable `grep' buffer
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package direnv
  :hook (after-init . direnv-mode))

(provide 'init-misc)
;;; init-misc.el ends here
