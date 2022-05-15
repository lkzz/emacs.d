;;; init-evil.el --- setup emacs use evil keybinds. -*- lexical-binding: t; -*-
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

(use-package evil
  :demand t ; https://github.com/noctuid/general.el/issues/180
  :hook (after-init . evil-mode)
  :init
  (setq evil-magic t
        evil-echo-state t
        evil-default-state 'normal
        evil-want-C-u-scroll t
        evil-want-C-w-delete t
        evil-want-C-i-jump t
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
        evil-undo-system 'undo-redo
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil
        evil-cross-lines t
        evil-move-cursor-back t ;; move back the cursor one position when exiting insert mode
        evil-ex-interactive-search-highlight 'selected-window ;; Only do highlighting in selected window so that Emacs has less work to do highlighting them all.
        evil-esc-delay 0.01)
  ;; cursor appearance
  (setq evil-default-cursor '(box (lambda () (evil-set-cursor-color my-default-cursor-color)))
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(bar (lambda () (evil-set-cursor-color my-emacs-cursor-color)))
        evil-insert-state-cursor '(bar . 2)
        evil-visual-state-cursor 'hollow)
  :config
  ;; completion state map
  (define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
  (define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)
  (define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
  ;; normal state map
  (define-key evil-normal-state-map "Y" (kbd "y$"))
  (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-normal-state-map ";d" #'kevin/delete-word)
  (define-key evil-normal-state-map ";y" #'kevin/copy-word)
  (define-key evil-normal-state-map ";w" #'save-buffer)
  (define-key evil-normal-state-map ";p" #'kevin/cover-word)
  ;; insert state map
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "M-j") 'yas-expand)
  ;; visual state map
  (define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)

  ;; Change the cursor color in emacs state. We do it this roundabout way
  ;; instead of changing `evil-default-cursor' (or `evil-emacs-state-cursor') so
  ;; it won't interfere with users who have changed these variables.
  (defvar my-default-cursor-color "#ffffff")
  (defvar my-emacs-cursor-color "#ff9999")
  (add-hook 'kevin-load-theme-hook (lambda ()
                                     (setq my-default-cursor-color (face-background 'cursor)
                                           my-emacs-cursor-color (face-foreground 'warning))))

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
    :init
    (evil-define-key '(normal visual) 'global (kbd "gcc")   #'evilnc-comment-or-uncomment-lines)
    (evil-define-key '(normal visual) 'global (kbd "gcp")   #'evilnc-comment-or-uncomment-paragraphs))

  (use-package evil-collection
    :custom (evil-collection-setup-minibuffer t)
    :init
    ;; The list of supported modes is configured by evil-collection-mode-list
    (evil-collection-init 'view)
    (evil-collection-init 'magit)
    (evil-collection-init 'custom)
    (evil-collection-init 'ibuffer)
    (evil-collection-init 'calendar)
    (evil-collection-init 'flycheck))

  (use-package evil-terminal-cursor-changer
    :unless (display-graphic-p)
    :config
    ;; cursor appearance in terminal
    (setq evil-default-cursor '(box (lambda () (evil-set-cursor-color my-default-cursor-color)))
          evil-normal-state-cursor 'box
          evil-emacs-state-cursor  '(hbar (lambda () (evil-set-cursor-color my-emacs-cursor-color)))
          evil-insert-state-cursor '(bar . 2)
          evil-visual-state-cursor 'hollow)
    (evil-terminal-cursor-changer-activate))

  ;; s: 2 char forward; S: 2 char backward
  ;; f: 1 char forward; F: 1 char backward
  ;; ;and, repeat search
  (use-package evil-snipe
    :hook ((evil-mode . evil-snipe-mode)
           (evil-mode . evil-snipe-override-mode))
    :diminish evil-snipe-local-mode
    :init
    (setq evil-snipe-smart-case t
          evil-snipe-scope 'line
          evil-snipe-repeat-scope 'visible
          evil-snipe-char-fold t)
    :config
    (add-to-list 'evil-snipe-disabled-modes 'Info-mode nil #'eq))

  (use-package evil-multiedit
    :config
    (evil-define-key 'normal 'global
      (kbd "M-d")   #'evil-multiedit-match-symbol-and-next
      (kbd "M-D")   #'evil-multiedit-match-symbol-and-prev)
    (evil-define-key 'visual 'global
      "R"           #'evil-multiedit-match-all
      (kbd "M-d")   #'evil-multiedit-match-and-next
      (kbd "M-D")   #'evil-multiedit-match-and-prev)
    (evil-define-key '(visual normal) 'global
      (kbd "C-M-d") #'evil-multiedit-restore)))

(provide 'init-evil)
;;; init-evil ends here
