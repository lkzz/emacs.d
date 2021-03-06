;;; init-misc.el --- misc config files. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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

;; Bookmark 设置
(use-package bookmark
  :ensure nil
  :general
  (kevin/space-key-define
    "m" '(nil :wk "bookmark")
    "m s" 'bookmark-set
    "m r" 'bookmark-rename
    "m d" 'bookmark-delete
    "m j" 'counsel-bookmark
    "m l" 'bookmark-bmenu-list))

;; Elec pair
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :config
  ;; 在minibuffer中禁止补全
  (setq electric-pair-inhibit-predicate (lambda (char) (minibufferp))))

;; Hungry deletion
(use-package hungry-delete
  :diminish hungry-delete-mode "ⓗ"
  :hook (after-init . global-hungry-delete-mode))

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
  :init
  (setq recentf-max-saved-items 500
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
  :init (setq avy-background t)
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

;; Move to the beginning/end of line or code
(use-package mwim
  :general ([remap move-beginning-of-line] 'mwim-beginning-of-code-or-line
            [remap move-end-of-line] 'mwim-end-of-code-or-line))

(use-package helpful
  :general
  ("C-h f" 'helpful-callable
   "C-h v" 'helpful-variable
   "C-h k" 'helpful-key))

;; Writable `grep' buffer
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package direnv
  :hook (after-init . direnv-mode))

(provide 'init-misc)
;;; init-misc.el ends here
