;;; init-keybinds.el --- personal keybinds,should be required finally.
;;; Commentary:
;;; Code:

;; global-keybindings
(global-set-key (kbd "C-M-\\") 'kevin/indent-region-or-buffer)
;; multiple cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; file
(evil-leader/set-key
  "ff"  'counsel-find-file
  "fr"  'counsel-recentf
  "fs"  'save-buffer
  "fed" 'eval-buffer
  "pf"  'counsel-projectile-find-file
  "p/"  'counsel-projectile-ag
  "pp"  'counsel-projectile-switch-project
  )

;; buffer
(evil-leader/set-key
  "bb" 'ivy-switch-buffer
  "bd" 'kill-this-buffer
  "bm" #'kevin/kill-all-buffers
  "bl" 'ibuffer-list-buffers
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bg" #'kevin/revert-buffer-no-confirm
  )

;; jump
(evil-leader/set-key
  "jd" 'dired-jump
  "jl" 'goto-line
  "jp" 'kevin/goto-match-parent
  "jc" 'avy-goto-char-2
  )

;; magit
(evil-leader/set-key
  "gc"  'magit-clone
  "gff" 'magit-find-file
  "gfh" 'magit-log-buffer-file
  "gi"  'magit-init
  "gL"  'magit-list-repositories
  "gm"  'magit-dispatch-popup
  "gs"  'magit-status
  "gS"  'magit-stage-file
  "gU"  'magit-unstage-file)

;; window
(evil-leader/set-key
  "0"  'select-window-0
  "1"  'select-window-1
  "2"  'select-window-2
  "3"  'select-window-3
  "4"  'select-window-4
  "5"  'select-window-5
  "6"  'select-window-6
  "7"  'select-window-7
  "8"  'select-window-8
  "9"  'select-window-9
  "wd" 'delete-window
  "w/" 'split-window-right
  "w-" 'split-window-below
  "wM" 'delete-other-windows)

;; treemacs
(evil-leader/set-key
  "ft" #'treemacs-toggle
  "fT" #'treemacs
  "fB" #'treemacs-bookmark
  "f C-t" #'treemacs-find-file)

;; application
(evil-leader/set-key
  "<SPC>" 'counsel-M-x
  "'"   'eshell
  "/"   'counsel-ag
  "ss"  'swiper)

;; bookmark
(evil-leader/set-key "ob" nil)
(evil-leader/set-key "obs" 'bookmark-set)
(evil-leader/set-key "obr" 'bookmark-rename)
(evil-leader/set-key "obd" 'bookmark-delete)
(evil-leader/set-key "obj" 'counsel-bookmark)
(evil-leader/set-key "obl" 'bookmark-bmenu-list)
;; toggle
(evil-leader/set-key "ot" nil)
(evil-leader/set-key "otm" 'toggle-major-mode)
(evil-leader/set-key "otb" 'toggle-scroll-bar)
(evil-leader/set-key "otw" 'toggle-word-wrap)
(evil-leader/set-key "otm" 'toggle-frame-maximized)
(evil-leader/set-key "otf" 'toggle-frame-fullscreen)
(evil-leader/set-key "otg" 'golden-ratio-mode)

;; open iterm2
(evil-leader/set-key "oi" 'kevin/iterm-focus)


;; flycheck error
(evil-leader/set-key "en" 'flycheck-next-error)
(evil-leader/set-key "ep" 'flycheck-previous-error)

(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "."  'evilnc-copy-and-comment-operator
  "\\" 'evilnc-comment-operator)

;; evil keybindings
(define-key evil-normal-state-map (kbd ",a") 'mwim-beginning-of-code-or-line)
(define-key evil-normal-state-map (kbd ",e") 'mwim-end-of-code-or-line)
(define-key evil-normal-state-map (kbd ",w") 'evil-write)
(define-key evil-normal-state-map (kbd ",W") 'evil-write-all)
(define-key evil-normal-state-map (kbd ",q") 'evil-quit)
(define-key evil-normal-state-map (kbd "C-w") 'evil-delete-backward-word)
(define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
(define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-motion-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-visual-state-map (kbd "C-e") 'end-of-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
(define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)

;; Magit from avsej
;;
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard
  "L" 'magit-log-popup)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard
  "l" 'magit-log-popup
  "h" 'magit-diff-toggle-refine-hunk)

(provide 'init-keybinds)
;;; init-keybinds ends here
