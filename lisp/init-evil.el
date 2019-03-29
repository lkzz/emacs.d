;;; init-evil.el --- setup emacs use evil keybinds. -*- lexical-binding: t; -*-
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

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-magic t
        evil-echo-state t
        evil-default-state 'normal
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-visual-char-semi-exclusive t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil
        evil-cross-lines t
        evil-move-cursor-back t ;; Move back the cursor one position when exiting insert mode
        evil-esc-delay 0.01
        evil-mode-line-format 'after)
  ;; ;; evil cursor color
  (setq  evil-default-cursor '("SkyBlue2" box)
         evil-normal-state-cursor '("SkyBlue2" box)
         evil-insert-state-cursor '("SkyBlue2" (bar . 2))
         evil-visual-state-cursor '("red" box)
         evil-replace-state-cursor '("#cd5c5c" box)
         evil-emacs-state-cursor '("#adfa2f" (bar . 2)))
  :config
  ;; evil ex command
  (evil-ex-define-cmd "W" 'evil-write-all)
  ;; evil insert,normal,visual,motion state keybinds
  (evil-define-key '(insert normal visual motion) 'global (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (evil-define-key '(insert normal visual motion) 'global (kbd "C-e") 'mwim-end-of-code-or-line)
  (evil-define-key '(insert normal visual motion) 'global (kbd "C-k") 'kill-whole-line)
  (evil-define-key '(insert normal visual motion) 'global (kbd "C-p") 'evil-previous-visual-line)
  (evil-define-key '(insert normal visual motion) 'global (kbd "C-n") 'evil-next-visual-line)
  ;; evil normal,visual,motion state keybinds
  (evil-define-key '(normal visual motion) 'global (kbd "j") 'evil-next-visual-line)
  (evil-define-key '(normal visual motion) 'global (kbd "k") 'evil-previous-visual-line)
  (evil-define-key '(normal visual motion) 'global (kbd "C-i") 'evil-jump-forward)
  (evil-define-key '(normal visual motion) 'global (kbd "C-o") 'evil-jump-backward)
  ;; evil normal state keybinds
  (evil-define-key 'normal 'global "Y" (kbd "y$"))
  (evil-define-key 'normal 'global (kbd ",w") 'evil-write)
  (evil-define-key 'normal 'global (kbd ",q") 'evil-quit)
  (evil-define-key 'normal 'global (kbd ",y") 'kevin/copy-word)
  (evil-define-key 'normal 'global (kbd ",p") 'kevin/cover-word)
  (evil-define-key 'normal 'global (kbd ",d") 'kevin/delete-word)
  (evil-define-key 'normal 'global (kbd "C-w") 'evil-delete-backward-word)
  ;; Use evil as a default jump handler
  (add-to-list 'kevin-default-jump-handlers 'evil-goto-definition))

(use-package evil-escape
  :after evil
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
  :after evil
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-nerd-commenter
  :after evil
  :init
  (kevin/set-leader-keys "ci" 'evilnc-comment-or-uncomment-lines
                         "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
                         "cp" 'evilnc-comment-or-uncomment-paragraphs
                         "cy" 'evilnc-copy-and-comment-operator))

;; s: 2 char forward; S: 2 char backward
;; f: 1 char forward; F: 1 char backward
;; ;and, repeat search
(use-package evil-snipe
  :after evil
  :hook ((evil-mode . evil-snipe-mode)
         (evil-mode . evil-snipe-override-mode))
  :diminish evil-snipe-local-mode
  :init
  (setq evil-snpe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (add-to-list 'evil-snipe-disabled-modes 'Info-mode nil #'eq))

(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :init
  ;; The list of supported modes is configured by evil-collection-mode-list
  (evil-collection-init 'view)
  (evil-collection-init 'custom)
  (evil-collection-init 'ibuffer)
  (evil-collection-init 'calendar))

(provide 'init-evil)
;;; init-evil ends here
