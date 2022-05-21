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


(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

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

(use-package savehist
  :straight (:type built-in)
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
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; A better *Help* buffer, from centaur emacs
(use-package helpful
  :commands helpful--buffer
  :bind (("C-c C-d" . helpful-at-point)
         ([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol))
  :hook (helpful-mode . cursor-sensor-mode) ; for remove-advice button
  :init
  (with-no-warnings
    (with-eval-after-load 'counsel
      (setq counsel-describe-function-function #'helpful-callable
            counsel-describe-variable-function #'helpful-variable
            counsel-describe-symbol-function #'helpful-symbol
            counsel-descbinds-function #'helpful-callable))

    (with-eval-after-load 'apropos
      ;; patch apropos buttons to call helpful instead of help
      (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
        (button-type-put
         fun-bt 'action
         (lambda (button)
           (helpful-callable (button-get button 'apropos-symbol)))))
      (dolist (var-bt '(apropos-variable apropos-user-option))
        (button-type-put
         var-bt 'action
         (lambda (button)
           (helpful-variable (button-get button 'apropos-symbol)))))))
  :config
  ;; Open the buffer in other window
  (defun my-helpful--navigate (button)
    "Navigate to the path this BUTTON represents."
    (find-file-other-window (substring-no-properties (button-get button 'path)))
    ;; We use `get-text-property' to work around an Emacs 25 bug:
    ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f7c4bad17d83297ee9a1b57552b1944020f23aea
    (-when-let (pos (get-text-property button 'position
                                       (marker-buffer button)))
      (helpful--goto-char-widen pos)))
  (advice-add #'helpful--navigate :override #'my-helpful--navigate))

;; Writable `grep' buffer
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package direnv
  :hook (after-init . direnv-mode)
  :config
  (setq direnv-always-show-summary nil))

(use-package restart-emacs
  :defer t
  :commands restart-emacs)

;; An alternative M-x interface for Emacs
(use-package amx
  :defer t
  :init (setq amx-history-length 10))

(use-package popup-kill-ring
  :defer t
  :bind ("M-y" . popup-kill-ring))
(provide 'init-misc)
;;; init-misc.el ends here
