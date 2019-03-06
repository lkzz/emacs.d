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

(require 'cl)

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :preface
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :init
  (setq evil-magic t
        evil-echo-state t
        evil-default-state 'normal
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
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
        evil-esc-delay 0
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

  ;; evil insert state keybinds
  (define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)

  ;; evil normal state keybinds
  (define-key evil-normal-state-map "Y" (kbd "y$"))
  (define-key evil-normal-state-map (kbd ",w") 'evil-write)
  (define-key evil-normal-state-map (kbd ",q") 'evil-quit)
  (define-key evil-normal-state-map (kbd ",y") 'kevin/copy-word)
  (define-key evil-normal-state-map (kbd ",p") 'kevin/cover-word)
  (define-key evil-normal-state-map (kbd ",d") 'kevin/delete-word)
  (define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-normal-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-normal-state-map (kbd "C-w") 'evil-delete-backward-word)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)

  ;; evil motion state keybinds
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
  (define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
  (define-key evil-motion-state-map (kbd "C-e") 'mwim-end-of-code-or-line)

  ;; evil visual state keybinds
  (define-key evil-visual-state-map (kbd "C-e") 'end-of-line)

  ;; Use evil as a default jump handler
  (add-to-list 'kevin-default-jump-handlers 'evil-goto-definition))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-visualstar
  :after evil
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :init
  (kevin/set-leader-keys "ci" 'evilnc-comment-or-uncomment-lines
                         "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
                         "cp" 'evilnc-comment-or-uncomment-paragraphs
                         "cy" 'evilnc-copy-and-comment-operator))

(use-package evil-escape
  :ensure t
  :after evil
  :diminish evil-escape-mode
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.3))

(use-package evil-mc
  :ensure t
  :after evil
  :diminish evil-mc-mode "ⓜ"
  :init
  (defun kevin/toggle-evil-mc ()
    (interactive)
    (if evil-mc-mode
        (progn
          (evil-mc-undo-all-cursors)
          (evil-mc-mode -1)
          (message "evil mc mode disabled"))
      (progn
        (evil-mc-mode 1)
        (message "evil mc mode enabled"))))
  (kevin/set-leader-keys "tm" #'kevin/toggle-evil-mc)
  (defun kevin/reset-evil-mc-key-map ()
    (let ((keys '(("ma" . evil-mc-make-all-cursors)
                  ("mu" . evil-mc-undo-all-cursors)
                  ("ms" . evil-mc-pause-cursors)
                  ("mr" . evil-mc-resume-cursors)
                  ("mf" . evil-mc-make-and-goto-first-cursor)
                  ("mb" . evil-mc-make-and-goto-last-cursor)
                  ("mh" . evil-mc-make-cursor-here)
                  ("mn" . evil-mc-skip-and-goto-next-match)
                  ("mp" . evil-mc-skip-and-goto-prev-match)
                  ("C-n" . evil-mc-make-and-goto-next-match)
                  ("C-p" . evil-mc-make-and-goto-prev-match)
                  )))
      (dolist (key-data keys)
        ;; (evil-define-key 'normal 'evil-mc-key-map (kbd (car key-data)) (cdr key-data))
        (evil-define-key 'visual 'evil-mc-key-map (kbd (car key-data)) (cdr key-data)))))
  :config
  (kevin/reset-evil-mc-key-map))

(use-package evil-snipe
  :ensure t
  :after evil
  :diminish evil-snipe-local-mode
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  ;; fix problems with magit buffer
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package vimish-fold
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (vimish-fold-global-mode t))

(use-package evil-vimish-fold
  :ensure t
  :after evil
  :commands evil-vimish-fold-mode
  :diminish evil-vimish-fold-mode
  :init
  (setq vimish-fold-dir (concat kevin-cache-directory "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  (add-hook 'after-init-hook #'evil-vimish-fold-mode t))

(use-package evil-collection
  :ensure t
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  ;; The list of supported modes is configured by evil-collection-mode-list
  (evil-collection-init 'view)
  (evil-collection-init 'custom)
  (evil-collection-init 'calendar))

(provide 'init-evil)
;;; init-evil ends here
