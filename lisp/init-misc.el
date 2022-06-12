;;; init-misc.el --- misc config files. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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
  :defer t
  :straight (:type built-in))

;; Elec pair
(use-package elec-pair
  :straight (:type built-in)
  :hook (after-init . electric-pair-mode)
  :config
  ;; 在minibuffer中禁止补全
  (setq electric-pair-inhibit-predicate (lambda (char) (minibufferp))))

;; Hungry deletion
(use-package hungry-delete
  :diminish hungry-delete-mode "ⓗ"
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; History
(use-package saveplace
  :straight (:type built-in)
  :hook (after-init . save-place-mode))

(use-package recentf
  :straight (:type built-in)
  :hook ((after-init . recentf-mode)
         (kill-emacs-hook . recentf-cleanup))
  :init
  (setq recentf-max-saved-items 200
        recentf-auto-cleanup 'never
        recentf-exclude '("/tmp/"
                          "/usr/local/Cellar/"
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
                          my-cache-dir
                          (lambda (file) (file-in-directory-p file package-user-dir))
                          (lambda (file) (file-in-directory-p file my-cache-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (setq recentf-filename-handlers
        (append '(abbreviate-file-name) recentf-filename-handlers)))

;; Jump to things in Emacs tree-style
(use-package avy
  :hook (after-init . avy-setup-default)
  :init (setq avy-background t))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; A better *Help* buffer.
(use-package helpful
  :commands helpful--buffer
  :bind (("C-c C-d" . helpful-at-point)
         ([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol))
  :hook (helpful-mode . cursor-sensor-mode)) ; for remove-advice button

(use-package direnv
  :hook (after-init . direnv-mode)
  :config
  (setq direnv-always-show-summary nil))

(use-package restart-emacs
  :commands restart-emacs)

;; An alternative M-x interface for Emacs
(use-package amx
  :defer t
  :init (setq amx-history-length 10))

(use-package youdao-dictionary
  :bind (("C-c y" . my/youdao-dictionary-search-at-point))
  :init (setq url-automatic-caching t
              youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词
  (defun my/youdao-dictionary-search-at-point ()
    "Search word at point and display result with `posframe' or `popup'"
    (interactive)
    (if (display-graphic-p)
        (youdao-dictionary-search-at-point-posframe)
      (youdao-dictionary-search-at-point+))))

(use-package rime
  :disabled
  :custom
  (rime-librime-root "~/Dropbox/librime/dist")
  (rime-user-data-dir "~/Dropbox/RimeSync")
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  :custom-face
  (rime-code-face ((t (:foreground "#ee6363"))))
  (rime-candidate-num-face ((t (:foreground "#ee6363"))))
  :config
  (setq rime-posframe-properties
        (list :font "STKaiti-16"
              :internal-border-width 10)))

(provide 'init-misc)
;;; init-misc.el ends here
