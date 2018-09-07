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
  :ensure t
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
  (define-key evil-normal-state-map (kbd ",q") 'evil-quit)
  (define-key evil-normal-state-map (kbd ",y") 'kevin/copy-word)
  (define-key evil-normal-state-map (kbd ",p") 'kevin/cover-word)
  (define-key evil-normal-state-map (kbd ",d") 'kevin/delete-word)
  (define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-normal-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
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
  :ensure t
  :after evil
  :init
  (kevin/set-leader-keys
   "ci" 'evilnc-comment-or-uncomment-lines
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
  :diminish evil-mc-mode
  :init
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
  (global-evil-mc-mode)
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

(provide 'init-evil)
;;; init-evil ends here
