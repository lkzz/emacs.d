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


(my-space-leader-def
  "SPC" 'amx
  "b" '(:ignore t :wk "buffer")
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
  "b g" '(kevin/revert-buffer-no-confirm :wk "revert-buffer")
  "b s" 'save-buffer
  "b S" '(kevin/create-scratch-buffer :wk "create-scratch-buffer")
  "e"   '(:ignore t :wk "error")
  "e l" 'flycheck-list-errors
  "e n" 'flycheck-next-error
  "e p" 'flycheck-previous-error
  "d"   '(:ignore t :wk "delete")
  "d d" '(kevin/delete-delimiter-enclosed-text :wk "delete-enclosed-text")
  "f"   '(:ignore t :wk "file")
  "f f" 'find-file
  "f i" '(kevin/open-init-file :wk "open-init-file")
  "f r" 'recentf
  "f z" 'counsel-fzf
  "g"   '(:ignore t :wk "git")
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
  "g g" 'magit-status
  "g S" 'magit-stage-file
  "g t" '(hydra-git-timemachine/body :wk "git-timemachine")
  "g u" 'magit-unstage-file
  "g v" 'vc-annotate
  "j"   '(:ignore t :wk "jump")
  "j c" 'avy-goto-char-2
  "j d" 'dired-jump
  "j f" 'beginning-of-defun
  "j j" 'scroll-other-window-down
  "j l" 'avy-goto-line
  "j m" '(kevin/jump-match-delimiter :wk "goto-match-delimiter")
  "j w" 'avy-goto-word-or-subword-1
  "k k" '(scroll-other-window :wk "scroll-other-window-up")
  "m"   '(:ignore t :wk "bookmark")
  "m s" 'bookmark-set
  "m r" 'bookmark-rename
  "m d" 'bookmark-delete
  "m j" 'counsel-bookmark
  "m l" 'bookmark-bmenu-list
  "p"   '(:ignore t :wk "projectile")
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
  "s"   '(:ignore t :wk "search")
  "s /" 'counsel-rg
  "s s" 'swiper-all
  "t"   '(:ignore t :wk "toggle")
  "t b" '(toggle-scroll-bar :wk "scroll-bar")
  "t d" '(kevin/toggle-darkroom-mode :wk "darkroom")
  "t f" '(neotree-toggle :wk "neotree")
  "t F" '(toggle-frame-fullscreen :wk "fullscreen")
  "t g" '(kevin/toggle-golden-ratio :wk "golden-ratio")
  "t i" 'maple-imenu
  "t l" '(toggle-truncate-lines :wk "truncate-line")
  "t s" 'symbol-overlay-mode
  "t t" 'vterm-toggle
  "1"  'winum-select-window-1
  "2"  'winum-select-window-2
  "3"  'winum-select-window-3
  "4"  'winum-select-window-4
  "5"  'winum-select-window-5
  "6"  'winum-select-window-6
  "7"  'winum-select-window-7
  "8"  'winum-select-window-8
  "9"  'winum-select-window-9
  "w"  '(nil :wk "window")
  "w d" 'delete-window
  "w o" 'other-window
  "w t" '(kevin/toggle-two-split-window :wk "toggle-two-split-window")
  "w z" '(zoom-window-zoom :wk "zoom-window")
  "w /" '(kevin/split-window-right-and-focus :wk "split-window-right")
  "w -" '(kevin/split-window-below-and-focus :wk "split-window-below")
  "w D" 'delete-other-windows)

(provide 'init-keybindings.el)

;;; init-keybindings.el ends here
