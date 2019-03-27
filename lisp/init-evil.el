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

(use-package evil-surround
  :after evil
  :custom (global-evil-surround-mode t))

;; search selection in visual state(*:forward #:forward).
(use-package evil-visualstar
  :after evil
  :custom (global-evil-visualstar-mode t))

(use-package evil-nerd-commenter
  :after evil
  :init
  (kevin/set-leader-keys "ci" 'evilnc-comment-or-uncomment-lines
                         "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
                         "cp" 'evilnc-comment-or-uncomment-paragraphs
                         "cy" 'evilnc-copy-and-comment-operator))

(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :custom (evil-escape-mode t)
  :config
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.25
        evil-escape-excluded-major-modes '(neotree-mode)
        evil-escape-excluded-states '(normal visual multiedit emacs motion))
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp))

;; (use-package evil-mc
;;   :diminish evil-mc-mode "ⓜ"
;;   :functions evil-mc-mode
;;   :commands (evil-mc-mode
;;              evil-mc-undo-all-cursors
;;              kevin/toggle-evil-mc)
;;   :preface
;;   (defun kevin/toggle-evil-mc ()
;;     (interactive)
;;     (if evil-mc-mode
;;         (progn
;;           (evil-mc-undo-all-cursors)
;;           (turn-off-evil-mc-mode)
;;           (message "evil mc mode disabled"))
;;       (progn
;;         (turn-on-evil-mc-mode)
;;         (message "evil mc mode enabled"))))
;;   :init
;;   (kevin/set-leader-keys "tm" #'kevin/toggle-evil-mc)
;;   ;; 设置在evil-mc之下可以执行的命令
;;   (setq evil-mc-custom-known-commands
;;         '((paredit-backward-delete . ((:default . evil-mc-execute-default-call-with-count)))
;;           (hungry-delete-backward . ((:default . evil-mc-execute-default-call-with-count)))
;;           (org-delete-backward-char . ((:default . evil-mc-execute-default-call-with-count)))))
;;   :config
;;   ;; FIXME 修复以下按键不生效的问题
;;   (with-eval-after-load 'evil
;;     ;; 清空evil-mc所有默认按键绑定
;;     (setq evil-mc-key-map (make-sparse-keymap))
;;     ;; 重新自定义按键绑定
;;     (evil-define-key 'visual evil-mc-key-map (kbd "ma") 'evil-mc-make-all-cursors)
;;     (evil-define-key '(normal visual) evil-mc-key-map (kbd "M-n")  'evil-mc-make-and-goto-next-match)
;;     (evil-define-key '(normal visual) evil-mc-key-map (kbd "M-p")  'evil-mc-make-and-goto-prev-match)
;;     (evil-define-key '(normal visual) evil-mc-key-map (kbd "C-n")  'evil-mc-skip-and-goto-next-match)
;;     (evil-define-key '(normal visual) evil-mc-key-map (kbd "C-p")  'evil-mc-skip-and-goto-prev-match)
;;     (evil-define-key 'normal evil-mc-key-map (kbd "ms") 'evil-mc-pause-cursors)
;;     (evil-define-key 'normal evil-mc-key-map (kbd "mh") 'evil-mc-make-cursor-here)
;;     (evil-define-key 'normal evil-mc-key-map (kbd "mu") 'evil-mc-undo-all-cursors)
;;     (evil-define-key 'normal evil-mc-key-map (kbd "mr") 'evil-mc-resume-cursors)
;;     (evil-define-key 'normal evil-mc-key-map (kbd "ma") 'evil-mc-make-and-goto-first-cursor)
;;     (evil-define-key 'normal evil-mc-key-map (kbd "me") 'evil-mc-make-and-goto-last-cursor)
;;     ))


;; s: 2 char forward; S: 2 char backward
;; f: 1 char forward; F: 1 char backward
;; ;and, repeat search
(use-package evil-snipe
  :after evil
  :diminish evil-snipe-local-mode
  :custom
  (evil-snipe-mode t)
  (evil-snipe-override-mode t)
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
  :config
  ;; The list of supported modes is configured by evil-collection-mode-list
  (evil-collection-init 'view)
  (evil-collection-init 'custom)
  (evil-collection-init 'calendar))

(provide 'init-evil)
;;; init-evil ends here
