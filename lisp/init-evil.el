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
  ;; evil normal,visual,motion state keybinds
  (evil-define-key '(normal visual motion) 'global (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (evil-define-key '(normal visual motion) 'global (kbd "C-e") 'mwim-end-of-code-or-line)
  (evil-define-key '(normal visual motion) 'global (kbd "C-k") 'kill-whole-line)
  (evil-define-key '(normal visual motion) 'global (kbd "C-p") 'evil-previous-visual-line)
  (evil-define-key '(normal visual motion) 'global (kbd "C-n") 'evil-next-visual-line)
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
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  ;; search selection in visual state(*:forward #:forward).
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
  :commands (evil-escape evil-escape-mode evil-escape-pre-command-hook)
  :init
  (setq evil-escape-key-sequence "jk"
        evil-escape-delay 0.25
        evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode))
  (add-hook 'pre-command-hook #'evil-escape-pre-command-hook)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  (evil-escape-mode))

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
  ;; 清空evil-mc所有默认按键绑定
  (setq evil-mc-key-map (make-sparse-keymap))
  ;; 重新自定义按键绑定
  (evil-define-key 'visual evil-mc-key-map (kbd "ma") 'evil-mc-make-all-cursors)
  (evil-define-key '(normal visual) evil-mc-key-map (kbd "M-n")  'evil-mc-make-and-goto-next-match)
  (evil-define-key '(normal visual) evil-mc-key-map (kbd "M-p")  'evil-mc-make-and-goto-prev-match)
  (evil-define-key '(normal visual) evil-mc-key-map (kbd "C-n")  'evil-mc-skip-and-goto-next-match)
  (evil-define-key '(normal visual) evil-mc-key-map (kbd "C-p")  'evil-mc-skip-and-goto-prev-match)
  (evil-define-key 'normal evil-mc-key-map (kbd "ms") 'evil-mc-pause-cursors)
  (evil-define-key 'normal evil-mc-key-map (kbd "mh") 'evil-mc-make-cursor-here)
  (evil-define-key 'normal evil-mc-key-map (kbd "mu") 'evil-mc-undo-all-cursors)
  (evil-define-key 'normal evil-mc-key-map (kbd "mr") 'evil-mc-resume-cursors)
  (evil-define-key 'normal evil-mc-key-map (kbd "ma") 'evil-mc-make-and-goto-first-cursor)
  (evil-define-key 'normal evil-mc-key-map (kbd "me") 'evil-mc-make-and-goto-last-cursor)
  ;; 设置在evil-mc之下可以执行的命令
  (setq evil-mc-custom-known-commands
        '((paredit-backward-delete . ((:default . evil-mc-execute-default-call-with-count)))
          (hungry-delete-backward . ((:default . evil-mc-execute-default-call-with-count)))
          (org-delete-backward-char . ((:default . evil-mc-execute-default-call-with-count)))
          ))
  )

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
  :hook (prog-mode . evil-vimish-fold-mode)
  :init
  (setq vimish-fold-dir (concat kevin-cache-directory "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe))

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
