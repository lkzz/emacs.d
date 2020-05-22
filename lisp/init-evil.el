;;; init-evil.el --- setup emacs use evil keybinds. -*- lexical-binding: t; -*-
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

(use-package evil
  :demand t ; https://github.com/noctuid/general.el/issues/180
  :hook (after-init . evil-mode)
  :init
  (setq evil-magic t
        evil-echo-state t
        evil-default-state 'normal
        evil-want-C-u-scroll t
        evil-want-C-w-delete t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-visual-char-semi-exclusive t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-disable-insert-state-bindings t ; enable default emacs keybinding in insert state
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil
        evil-cross-lines t
        evil-move-cursor-back t ;; move back the cursor one position when exiting insert mode
        evil-esc-delay 0.01
        evil-mode-line-format 'after)
  ;; ;; evil cursor color
  (setq  evil-default-cursor '("red" box)
         evil-normal-state-cursor '("red" box)
         evil-insert-state-cursor '("red" (bar . 2))
         evil-visual-state-cursor '("red" box)
         evil-replace-state-cursor '("red" hollow)
         evil-emacs-state-cursor '("red" hbar))
  :config
  (general-nvmap "C-e" 'move-end-of-line)
  (general-nmap
    "Y" (kbd "y$")
    "gr" 'xref-find-references)
  (define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)
  (define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)

  (use-package evil-escape
    :diminish evil-escape-mode
    :hook (evil-mode . evil-escape-mode)
    :config
    (setq  evil-escape-delay 0.25
           evil-escape-key-sequence "jk"
           evil-escape-excluded-major-modes '(neotree-mode)
           evil-escape-excluded-states '(normal visual multiedit emacs motion))
    ;; no `evil-escape' in minibuffer
    (add-hook 'evil-escape-inhibit-functions #'minibufferp))

  (use-package evil-surround
    :hook (evil-mode . global-evil-surround-mode))

  (use-package evil-nerd-commenter
    :general
    (kevin/space-key-define
      "c" '(nil :which-key "Comment")
      "c i" '(evilnc-comment-or-uncomment-lines :wk "comment-lines")
      "c l" '(evilnc-quick-comment-or-uncomment-to-the-line :wk "comment-line")
      "c p" '(evilnc-comment-or-uncomment-paragraphs :wk "comment paragraphs")
      "c y" '(evilnc-copy-and-comment-operator :wk "comment-and-copy")))

  (use-package evil-collection
    :custom (evil-collection-setup-minibuffer t)
    :init
    ;; The list of supported modes is configured by evil-collection-mode-list
    (evil-collection-init 'view)
    (evil-collection-init 'custom)
    (evil-collection-init 'ibuffer)
    (evil-collection-init 'calendar))

  (use-package evil-terminal-cursor-changer
    :unless (display-graphic-p)
    :config
    (setq evil-motion-state-cursor 'box
          evil-visual-state-cursor 'box
          evil-normal-state-cursor 'box
          evil-insert-state-cursor 'bar
          evil-emacs-state-cursor  'hbar)
    (evil-terminal-cursor-changer-activate))

  ;; s: 2 char forward; S: 2 char backward
  ;; f: 1 char forward; F: 1 char backward
  ;; ;and, repeat search
  (use-package evil-snipe
    :hook ((evil-mode . evil-snipe-mode)
           (evil-mode . evil-snipe-override-mode))
    :diminish evil-snipe-local-mode
    :init
    (setq evil-snpe-smart-case t
          evil-snipe-scope 'line
          evil-snipe-repeat-scope 'visible
          evil-snipe-char-fold t)
    :config
    (add-to-list 'evil-snipe-disabled-modes 'Info-mode nil #'eq)))

(provide 'init-evil)
;;; init-evil ends here
