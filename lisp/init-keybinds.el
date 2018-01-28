;;; init-keybinds.el --- personal keybinds,should be required finally.
;;; Commentary:
;;; Code:

;; global-keybindings
(global-set-key (kbd "C-M-\\") 'kevin/indent-region-or-buffer)

(evil-leader/set-leader "<SPC>")

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
  "bl" 'ibuffer-list-buffers
  "bp" 'previous-buffer
  "bn" 'next-buffer
  "bg" 'revert-buffer)

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

;; open iterm2
(evil-leader/set-key "oi" 'kevin/iterm-focus)


;; flycheck error
(evil-leader/set-key "en" 'flycheck-next-error)
(evil-leader/set-key "ep" 'flycheck-previous-error)

(provide 'init-keybinds)
;;; init-keybinds ends here
