;;; init-evil.el --- setup emacs use evil keybinds. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :defer t
  :hook (after-init . evil-mode)
  :config
  (setq evil-default-state 'normal)
  (setq evil-magic t
        evil-echo-state t
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
  ;; evil cursor color
  (setq  evil-default-cursor '("SkyBlue2" box)
         evil-normal-state-cursor '("SkyBlue2" box)
         evil-insert-state-cursor '("red" (bar . 2))
         evil-visual-state-cursor '("#98f5ff" box)
         evil-replace-state-cursor '("#cd5c5c" box)
         evil-operator-state-cursor '("#98f5ff" box)
         evil-motion-state-cursor '("#98f5ff" box)
         evil-emacs-state-cursor '("#adfa2f" (bar . 2)))
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
  (define-key evil-normal-state-map (kbd ",W") 'evil-write-all)
  (define-key evil-normal-state-map (kbd ",q") 'evil-quit)
  (define-key evil-normal-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-normal-state-map (kbd "C-w") 'evil-delete-backward-word)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; evil motion state keybinds
  (define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
  (define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
  (define-key evil-motion-state-map (kbd "C-e") 'mwim-end-of-code-or-line)

  ;; evil visual state keybinds
  (define-key evil-visual-state-map (kbd "C-e") 'end-of-line)

  ;; set evil state for major mode
  (require 'cl)
  (loop for (mode . state) in
        '(
          (view-mode . motion)
          )
        do (evil-set-initial-state mode state))
  )

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-visualstar
  :after evil
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

(use-package evil-numbers
  :ensure t
  :after evil)

(use-package evil-nerd-commenter
  :after evil
  :init
  (kevin/set-leader-keys
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cy" 'evilnc-copy-and-comment-operator))

(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.3))

(use-package evil-mc
  :defer t
  :after evil
  :diminish evil-mc-mode
  :config
  (global-evil-mc-mode 1)
  (use-package ace-mc)
  ;; multiple-cursors
  ;; step 1, select thing in visual-mode (OPTIONAL)
  ;; step 2, `mc/mark-all-like-dwim' or `mc/mark-all-like-this-in-defun'
  ;; step 3, `ace-mc-add-multiple-cursors' to remove cursor, press RET to confirm
  ;; step 4, press s or S to start replace
  ;; step 5, press C-g to quit multiple-cursors
  (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
  (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this-dwim)
  (define-key evil-visual-state-map (kbd "md") 'mc/mark-all-like-this-in-defun)
  (define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
  (define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor))


(provide 'init-evil)
;;; init-evil ends here
