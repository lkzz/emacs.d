;;; init-evil.el --- setup emacs use evil keybinds
;;; Commentary:
;;; Code:

(use-package evil-leader
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :config
  (evil-mode t)
  (setq evil-default-state 'normal)
  (setq evil-magic t
        evil-echo-state t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil)
  (setq evil-esc-delay 0)
  (setq evil-mode-line-format 'before)
  ;; modeline UI
  (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
        evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
        evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
        evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
        evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
        evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))
  ;; evil cursor color
  (setq  evil-default-cursor '("#98f5ff" box)
         evil-normal-state-cursor '("#98f5ff" box)
         evil-insert-state-cursor '("#98f5ff" (bar . 2))
         evil-visual-state-cursor '("#98f5ff" box)
         evil-replace-state-cursor '("#cd5c5c" box)
         evil-operator-state-cursor '("#98f5ff" box)
         evil-motion-state-cursor '("#98f5ff" box)
         evil-emacs-state-cursor '("#adfa2f" (bar . 2)))
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode t))

(use-package evil-numbers)

;; Scrolling
(defun prelude-evil-scroll-down-other-window ()
  (interactive)
  (scroll-other-window))

(use-package evil-nerd-commenter
  :after evil-mode
  :config
  (evil-leader/set-key
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "."  'evilnc-copy-and-comment-operator
    "\\" 'evilnc-comment-operator ; if you prefer backslash key
    ))

(provide 'init-evil)
;;; init-evil ends here
