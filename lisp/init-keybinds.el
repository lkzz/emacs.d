;;; init-keybinds.el --- personal keybinds,should be required finally.
;;; Commentary:
;;; Code:

;; file
(evil-leader/set-key
  "f f" 'counsel-find-file
  "f r" 'ivy-recentf
  "f s" 'save-buffer
  "f e d" 'eval-buffer
  "p f" 'project-find-file)

;; buffer
(evil-leader/set-key
  "b b" 'ivy-switch-buffer
  "b d" 'kill-this-buffer
  "b l" 'ibuffer-list-buffers
  "b g" 'revert-buffer)

;; jump
(evil-leader/set-key
  "j d" 'dired-jump
  "j l" 'goto-line
  "j p" 'kevin/goto-match-parent)

;; window
(evil-leader/set-key
  "0"  'select-window-0
  "1"  'select-window-1
  "2"  'select-window-2
  "3"  'select-window-3
  "w /" 'split-window-right
  "w -" 'split-window-below
  ":"  'counsel-M-x
  "w M" 'delete-other-windows)

;; magit
(evil-leader/set-key
  "g s" 'magit-status)

;; application
(evil-leader/set-key
  "/" 'eshell)

;; bookmark
(evil-leader/set-key "ob" nil)
(evil-leader/set-key "obs" 'bookmark-set)
(evil-leader/set-key "obr" 'bookmark-rename)
(evil-leader/set-key "obd" 'bookmark-delete)
(evil-leader/set-key "obj" 'helm-filtered-bookmarks)
(evil-leader/set-key "obl" 'bookmark-bmenu-list)
;; toggle
(evil-leader/set-key "ot" nil)
(evil-leader/set-key "otm" 'toggle-major-mode)
(evil-leader/set-key "otb" 'toggle-scroll-bar)
(evil-leader/set-key "otw" 'toggle-word-wrap)
;; open iterm2
(evil-leader/set-key "oi" 'kevin/iterm-focus)


;; flycheck error
(evil-leader/set-key "en" 'flycheck-next-error)
(evil-leader/set-key "ep" 'flycheck-previous-error)


(provide 'init-keybinds)
;;; init-keybinds ends here
