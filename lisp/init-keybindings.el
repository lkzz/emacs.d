;;; init-keybindings.el --- insert description here -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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

;; used as tmux prefix key
(global-unset-key (kbd "C-q"))
(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up   1)))
;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(when is-mac-p
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-right-option-modifier 'nil
        mac-right-option-modifier 'super)
  (global-set-key [(super a)] 'mark-whole-buffer)
  (global-set-key [(super v)] 'yank)
  (global-set-key [(super c)] 'kill-ring-save)
  (global-set-key [(super s)] 'save-buffer)
  (global-set-key [(super w)] (lambda () (interactive) (delete-window)))
  (global-set-key [(super z)] 'undo))

(use-package general
  :config
  (general-create-definer my-leader-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix my-leader-key-prefix)
  (general-create-definer my-local-leader-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix my-local-leader-key-prefix)
  (my-leader-define
    "SPC" '(execute-extended-command :wk "M-x")
    "b" '(:ignore t :wk "Buffer")
    "bb" '(counsel-switch-buffer :wk "Switch buffer")
    "bc" '(my-create-scratch-buffer :wk "Create buffer")
    "bf" '(format-all-buffer :wk "Format buffer")
    "bi" '(my-indent-region-or-buffer :wk "Indent buffer")
    "bk" '(kill-current-buffer :wk "Kill buffer")
    "bK" '(my-kill-other-buffers :wk "Kill other buffer")
    "bA" '(my-kill-all-buffers :wk "Kill all buffer")
    "bp" '(my-switch-to-prev-buffer :wk "Prev buffer")
    "bn" '(my-switch-to-next-buffer :wk "Next buffer")
    "bg" '(my-revert-buffer-no-confirm :wk "Revert buffer")
    "bs" '(basic-save-buffer :wk "Save buffer")
    "bS" '(evil-write-all :wk "Save all buffer")
    "c" '(:wk "Code")
    "cr" '(lsp-bridge-rename :wk "Rename")
    "cm" '(symbol-overlay-put :wk "Symbol Mark")
    "e"   '(:ignore t :wk "Error")
    "eb" '(flycheck-buffer :wk "Check current buffer")
    "el" '(counsel-flycheck :wk "List errors")
    "en" '(flycheck-next-error :wk "Next error")
    "ep" '(flycheck-previous-error :wk "Previous error")
    "ev" '(flycheck-verify-setup :wk "Verify setup")
    "es" '(flycheck-select-checker :wk "Select checker")
    "f"   '(:ignore t :wk "File")
    "fc" '(my-copy-file :wk "Copy file")
    "fd" '(my-delete-file :wk "Delete file")
    "ff" '(counsel-fzf :wk "Fuzzy find file")
    "fh" '(counsel-find-file :wk "Find file here")
    "fi" '(my-open-init-file :wk "Open init.el")
    "fr" '(counsel-recentf :wk "Recent file")
    "fs" '(my-save-file :wk "Save file")
    "fR" '(my-rename-file :wk "Rename file")
    "g"   '(:ignore t :wk "Git")
    "ga" '(my-git-add-current-file :wk "add-current-file")
    "gb" '(magit-branch-checkout :wk "Checkout branch")
    "gB" '(magit-blame :wk "Magit blame")
    "gc" '(my-git-checkout-current-file :wk "Checkout current file")
    "gd" '(magit-diff-buffer-file :wk "Diff current file")
    "gh" '(hydra-diff-hl/body :wk "hydra-diff-hl")
    "gl" '(magit-log-buffer-file :wk "Magit buffer log")
    "gL" '(magit-log :wk "Magit log")
    "gm" '(git-messenger:popup-message :wk "Popup message")
    "gr" '(hydra-smerge-mode/body :wk "hydra-smerge-mode")
    "gg" '(magit-status :wk "Status")
    "gs" '(magit-stage-file :wk "Stage file")
    "gt" '(hydra-git-timemachine/body :wk "Git timemachine")
    "gu" 'magit-unstage-file
    "gv" 'vc-annotate
    "j"   '(:ignore t :wk "Jump")
    "jc" 'avy-goto-char-2
    "jd" 'dired-jump
    "jf" 'beginning-of-defun
    "jj" 'scroll-other-window-down
    "jl" 'avy-goto-line
    "jm" '(my-jump-match-delimiter :wk "goto-match-delimiter")
    "jw" 'avy-goto-word-or-subword-1
    "m"   '(:ignore t :wk "Bookmark")
    "ms" 'bookmark-set
    "mr" 'bookmark-rename
    "md" 'bookmark-delete
    "mj" 'counsel-bookmark
    "ml" 'bookmark-bmenu-list
    "p"  '(:ignore t :wk "Project")
    "p!" 'projectile-run-shell-command-in-root
    "p%" 'projectile-replace-regexp
    "p/" 'projectile-ripgrep
    "pb" 'projectile-switch-to-buffer
    "pd" 'projectile-find-dir
    "pf" 'projectile-find-file
    "pg" 'projectile-find-tag
    "pk" 'projectile-kill-buffers
    "pp" 'projectile-switch-project
    "pr" 'projectile-recentf
    "pT" 'projectile-test-project
    "s"   '(:ignore t :wk "Search")
    "sh" '((lambda() (interactive) (counsel-rg nil default-directory)) :wk "Search current directory")
    "s/" '(counsel-rg :wk "Search ripgrep")
    "st" '(counsel-load-theme :wk "Search theme")
    "t"   '(:ignore t :wk "Toggle")
    "tb" '(toggle-scroll-bar :wk "scroll-bar")
    "td" '(my-toggle-darkroom-mode :wk "darkroom")
    "tf" '(neotree-toggle :wk "neotree")
    "tF" '(toggle-frame-fullscreen :wk "fullscreen")
    "tg" '(my-toggle-golden-ratio :wk "golden-ratio")
    "ti" 'maple-imenu
    "tl" '(toggle-truncate-lines :wk "truncate-line")
    "ts" 'symbol-overlay-mode
    "tt" 'vterm-toggle
    "1"  'winum-select-window-1
    "2"  'winum-select-window-2
    "3"  'winum-select-window-3
    "4"  'winum-select-window-4
    "5"  'winum-select-window-5
    "6"  'winum-select-window-6
    "7"  'winum-select-window-7
    "8"  'winum-select-window-8
    "9"  'winum-select-window-9
    "w"  '(nil :wk "Window")
    "wd" '(delete-window :wk "Delete window")
    "wD" '(delete-other-windows :wk "Delete other Window")
    "wo" '(other-window :wk "Other window")
    "wt" '(my-toggle-two-split-window :wk "toggle-two-split-window")
    "wz" '(zoom-window-zoom :wk "Zoom window")
    "w/" '(my-split-window-right-and-focus :wk "Split window right")
    "w-" '(my-split-window-below-and-focus :wk "Split window below")
    "w=" '(balance-windows :wk "Balance window")
    "q" '(:wk "Quit")
    "qq" '(save-buffers-kill-terminal :wk "Quit")
    "qr" '(restart-emacs :wk "Restart")))

(provide 'init-keybindings.el)
;;; init-keybindings.el ends here
