;;; core-keybind.el --- core keybinds -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017-2020  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;; Code:

(when is-mac-p
  (setq mac-command-modifier 'super) ; make Super key do command
  (setq mac-option-modifier 'meta)  ; make Option key do meta
  (setq mac-right-option-modifier 'none) ;; disable right Option key
  (setq mac-control-modifer 'control)
  (global-set-key [(super a)] 'mark-whole-buffer)
  (global-set-key [(super v)] 'yank)
  (global-set-key [(super c)] 'kill-ring-save)
  (global-set-key [(super s)] 'save-buffer)
  (global-set-key [(super w)]
                  (lambda () (interactive) (delete-window)))
  (global-set-key [(super z)] 'undo))

;; used as tmux prefix key
(global-unset-key (kbd "C-q"))
(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key   [mouse-4] '(lambda () (interactive) (scroll-down 1)))
(global-set-key   [mouse-5] '(lambda () (interactive) (scroll-up   1)))

(use-package which-key
  :diminish which-key-mode "Ⓚ"
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.3
        which-key-compute-remaps t
        which-key-min-display-lines 1
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-sort-uppercase-first nil
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.25
        which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-setup-side-window-bottom)
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
  ;; rename winum-select-window-1 entry to 1..9
  (add-to-list 'which-key-replacement-alist '(("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9")))
  ;; hide winum-select-window-[2-9] entries
  (add-to-list 'which-key-replacement-alist '((nil . "winum-select-window-[2-9]") . t))
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))

(use-package general
  :init
  (general-evil-setup)
  (general-create-definer kevin/space-key-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer kevin/comma-key-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix ",")

  (general-create-definer kevin/colon-key-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix ";"))

(defun kevin/init-default-keybinds ()
  (kevin/space-key-define
    "1"  'winum-select-window-1
    "2"  'winum-select-window-2
    "3"  'winum-select-window-3
    "4"  'winum-select-window-4
    "5"  'winum-select-window-5
    "6"  'winum-select-window-6
    "7"  'winum-select-window-7
    "8"  'winum-select-window-8
    "9"  'winum-select-window-9
    "SPC" 'amx
    "b" '(:ignore t :wk "Buffer")
    "b b" 'switch-to-buffer
    "b c" '(kevin/cleanup-buffer :wk "cleanup-buffer")
    "b e" 'eval-buffer
    "b d" 'kill-this-buffer
    "b D" '(kevin/kill-other-buffers :wk "kill-other")
    "b i" '(kevin/indent-region-or-buffer :wk "indent-buffer")
    "b k" 'kill-buffer
    "b l" 'ibuffer-list-buffers
    "b m" '(kevin/kill-all-buffers :wk "kill-all-buffer")
    "b p" '(kevin/switch-to-prev-buffer :wk "prev-buffer")
    "b n" '(kevin/switch-to-next-buffer :wk "next-buffer")
    "b r" 'reveal-in-osx-finder
    "b g" '(kevin/revert-buffer-no-confirm :wk "revert-buffer")
    "b s" 'save-buffer
    "b S" '(kevin/create-scratch-buffer :wk "create-scratch-buffer")
    "c" '(nil :which-key "Comment")
    "c i" '(evilnc-comment-or-uncomment-lines :wk "comment-lines")
    "c l" '(evilnc-quick-comment-or-uncomment-to-the-line :wk "comment-line")
    "c p" '(evilnc-comment-or-uncomment-paragraphs :wk "comment paragraphs")
    "c y" '(evilnc-copy-and-comment-operator :wk "comment-and-copy")
    "d" '(nil :which-key "Delete")
    "d d" '(kevin/delete-delimiter-enclosed-text :wk "delete-enclosed-text")
    "d f" 'delete-frame
    "d w" '(kevin/delete-word :wk "delete-word")
    "e" '(nil :which-key "Errors")
    "e l" 'flycheck-list-errors
    "e n" 'flycheck-next-error
    "e p" 'flycheck-previous-error
    "f" '(nil :which-key "File")
    "f r" 'recentf-open-files
    "f f" 'find-file
    "g" '(:ignore t :which-key "Git")
    "g a" '(kevin/git-add-current-file :wk "add-current-file")
    "g b" 'magit-blame
    "g c" '(kevin/git-checkout-current-file :wk "checkout-current-file")
    "g d" 'magit-diff-buffer-file
    "g h" '(hydra-diff-hl/body :wk "hydra-diff-hl")
    "g i" 'magit-init
    "g l" 'magit-log-buffer-file
    "g L" 'magit-list-repositories
    "g m" '(git-messenger:popup-message :wk "popup-message")
    "g r" '(hydra-smerge-mode/body :wk "hydra-smerge-mode")
    "g s" 'magit-status
    "g S" 'magit-stage-file
    "g t" '(hydra-git-timemachine/body :wk "git-timemachine")
    "g u" 'magit-unstage-file
    "g v" 'vc-annotate
    "h" '(:ignore t :wk "help")
    "h f" 'helpful-callable
    "h v" 'helpful-variable
    "h k" 'helpful-key
    "j" '(nil :which-key "Jump")
    "j c" 'avy-goto-char-2
    "j d" 'dired-jump
    "j f" 'beginning-of-defun
    "j l" 'avy-goto-line
    "j m" '(kevin/jump-match-delimiter :wk "goto-match-delimiter")
    "j w" 'avy-goto-word-or-subword-1
    "m" '(nil :which-key "Bookmark")
    "m s" 'bookmark-set
    "m r" 'bookmark-rename
    "m d" 'bookmark-delete
    "m j" 'counsel-bookmark
    "m l" 'bookmark-bmenu-list
    "o" '(nil :which-key "Open")
    "o a" 'counsel-osx-app
    "o t" '(kevin/open-iterm :wk "open-item2")
    "o w" '(kevin/open-wechat :wk "open-wechat")
    "o y" '(kevin/open-youdao :wk "open-youdao")
    "p" '(nil :which-key "Projectile")
    "p !" 'projectile-run-shell-command-in-root
    "p &" 'projectile-run-async-shell-command-in-root
    "p %" 'projectile-replace-regexp
    "p /" 'projectile-ripgrep
    "p a" 'projectile-toggle-between-implementation-and-test
    "p b" 'projectile-switch-to-buffer
    "p c" 'projectile-compile-project
    "p d" 'projectile-find-dir
    "p D" 'projectile-dired
    "p f" 'projectile-find-file
    "p F" 'projectile-find-file-dwim
    "p g" 'projectile-find-tag
    "p G" 'projectile-regenerate-tags
    "p I" 'projectile-invalidate-cache
    "p k" 'projectile-kill-buffers
    "p p" 'projectile-switch-project
    "p r" 'projectile-recentf
    "p R" 'projectile-replace
    "p T" 'projectile-test-project
    "p v" 'projectile-vc
    "s" '(nil :which-key "Search")
    "s /" 'counsel-rg
    "s s" 'swiper-all
    "t" '(nil :which-key "Toggle")
    "t ;" 'toggle-frame-fullscreen
    "t b" 'toggle-scroll-bar
    "t f" 'neotree-toggle
    "t g" '(kevin/toggle-golden-ratio :wk "golden-ratio")
    "t i" 'maple-imenu
    "t s" 'symbol-overlay-mode
    "t t" '(kevin/toggle-aweshell :wk "shell")
    "w" '(nil :which-key "Window")
    "w c" 'centered-window-mode
    "w d" 'delete-window
    "w o" 'other-window
    "w d" 'delete-window
    "w z" 'zoom-window-zoom
    "w /" '(kevin/split-window-right-and-focus :wk "split-window-right")
    "w -" '(kevin/split-window-below-and-focus :wk "split-window-below")
    "w D" 'delete-other-windows)

  (kevin/comma-key-define
    "f" '(nil :which-key "Find")
    "f d" 'xref-find-definitions
    "f f" 'find-file-at-point
    "f r" 'xref-find-references
    "i" '(nil :which-key "Import")
    "i a" 'go-import-add
    "i g" 'go-goto-imports
    "i r" 'go-remove-unused-imports
    "t" '(nil :which-key "Test")
    "t x" 'go-run
    "t b" 'go-test-current-benchmark
    "t t" 'go-test-current-test
    "t f" 'go-test-current-file
    "t p" 'go-test-current-project
    "d" '(kevin/delete-word :wk "delete-word")
    "y" '(kevin/copy-word :wk "copy-word")
    "p" '(kevin/cover-word :wk "cover-word"))

  (kevin/colon-key-define
    "e" '(nil :which-key "Emacs")
    "e r" 'restart-emacs
    "e q" 'save-buffers-kill-terminal
    "e i" '(kevin/open-init-file :wk "open-init-file")
    "f" '(nil :which-key "Font")
    "f =" 'kevin/increase-fontsize
    "f -" 'kevin/decrease-fontsize)

  (general-nvmap dashboard-mode-map
    "TAB" 'widget-forward
    "RET" 'widget-button-press
    "g" 'dashboard-refresh-buffer
    "}" 'dashboard-next-section
    "{" 'dashboard-previous-section
    "p" 'kevin/dashboard-goto-projects
    "m" 'kevin/dashboard-goto-bookmarks
    "r" 'kevin/dashboard-goto-recent-files
    "H" 'kevin/browse-homepage
    "R" 'kevin/restore-session
    "O" 'kevin/dashboard-open-init-file
    "q" 'kevin/quit-dashboard)

  (general-nmap dired-mode-map
    ;; Lower keys for commands not operating on all the marked files
    "a" 'dired-find-alternate-file
    "d" 'dired-flag-file-deletion
    "gf" 'dired-find-file
    "gy" 'dired-show-file-type
    "gr" 'revert-buffer
    "h" 'dired-up-directory
    "i" 'dired-toggle-read-only
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "l" 'dired-find-file
    "m" 'dired-mark
    "o" 'dired-sort-toggle-or-edit
    "q" 'quit-window
    "r" 'dired-do-redisplay
    "th" 'dired-omit-mode
    "tt" 'dired-toggle-marks
    "u" 'dired-unmark
    "v" 'dired-git-info-mode
    "x" 'dired-do-flagged-delete
    "RET" 'dired-find-file
    ;; Commands to mark or flag certain categories of files
    "+" 'dired-create-directory
    "^" 'dired-up-directory
    "#" 'dired-flag-auto-save-files
    "." 'dired-clean-directory
    "~" 'dired-flag-backup-files
    "!" 'dired-do-shell-command
    "&" 'dired-do-async-shell-command
    ;; Upper case keys (except !) for operating on the marked files
    "A" 'dired-do-find-regexp
    "C" 'dired-do-copy
    "B" 'dired-do-byte-compile
    "D" 'dired-do-delete
    "G" 'dired-do-chgrp
    "H" 'dired-do-hardlink
    "I" 'dired-maybe-insert-subdir
    "J" 'dired-goto-file
    "K" 'dired-do-kill-lines
    "L" 'dired-do-load
    "M" 'dired-do-chmod
    "O" 'dired-do-chown
    "P" 'dired-do-print
    "Q" 'dired-do-find-regexp-and-replace
    "R" 'dired-do-rename
    "S" 'dired-do-symlink
    "T" 'dired-do-touch
    "W" 'browse-url-of-dired-file
    "X" 'dired-do-shell-command
    "Y" 'dired-copy-filename-as-kill
    "Z" 'dired-do-compress)

  (general-nmap neotree-mode-map
    "RET" 'neotree-enter
    "o" 'neotree-enter
    "q" 'neotree-hide
    "h" 'neotree-select-up-node
    "l" 'neotree-change-root
    "c" 'neotree-create-node
    "C" 'neotree-copy-node
    "d" 'neotree-delete-node
    "g" 'neotree-refresh
    "r" 'neotree-rename-node
    "th" 'neotree-hidden-file-toggle))

(provide 'core-keybind)
;;; core-keybind.el ends here
