;;; init-evil.el --- setup emacs use evil keybinds
;;; Commentary:
;;; Code:

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :config
  (progn
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
          shift-select-mode nil)
    (setq evil-mode-line-format nil)
    (setq evil-cross-lines t)
    (setq evil-move-cursor-back t) ;; Move back the cursor one position when exiting insert mode
    (setq evil-esc-delay 0)
    (setq evil-mode-line-format 'before)

    ;; modeline UI
    (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "green" :foreground "black")))
          evil-emacs-state-tag    (propertize "[E]" 'face '((:background "orange" :foreground "black")))
          evil-insert-state-tag   (propertize "[I]" 'face '((:background "red")))
          evil-motion-state-tag   (propertize "[M]" 'face '((:background "blue")))
          evil-visual-state-tag   (propertize "[V]" 'face '((:background "grey80" :foreground "black")))
          evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

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
    ;; (define-key evil-normal-state-map (kbd ",w") 'evil-write)
    ;; (define-key evil-normal-state-map (kbd ",W") 'evil-write-all)
    (define-key evil-normal-state-map (kbd ",q") 'evil-quit)
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

    ))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (progn
    (setq evil-visualstar/persistent t)
    (global-evil-visualstar-mode)))

(use-package evil-numbers
  :ensure t
  :defer t)

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :init
  (progn
    (evil-leader/set-key
      "ci" 'evilnc-comment-or-uncomment-lines
      "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cy" 'evilnc-copy-and-comment-operator
      )))

(use-package evil-escape
  :ensure t
  :after evil
  :diminish evil-escape-mode
  :config
  (progn
    (evil-escape-mode)
    (setq-default evil-escape-key-sequence "jk")
    (setq-default evil-escape-delay 0.3)))


(provide 'init-evil)
;;; init-evil ends here
