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
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-=") 'text-scale-increase)

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up   1)))
;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(when is-mac-p (setq mac-command-modifier 'meta
                     mac-option-modifier 'alt)
      (global-set-key [(meta a)] 'mark-whole-buffer)
      (global-set-key [(meta v)] 'yank)
      (global-set-key [(meta c)] 'kill-ring-save)
      (global-set-key [(meta s)] 'save-buffer)
      (global-set-key [(meta z)] 'undo))

(define-key global-map (kbd "M-y") 'yank-pop)

(my/global-leader-define
  "SPC" '(execute-extended-command :wk "M-x")
  "b"  '(:ignore t :wk "Buffer")
  "bb" '(switch-to-buffer :wk "Switch buffer")
  "bB" '(switch-to-buffer-other-window :wk "Switch buffer other window")
  "bc" '(my/create-scratch-buffer :wk "Create buffer")
  "bk" '(kill-current-buffer :wk "Kill buffer")
  "bK" '(my/kill-other-buffers :wk "Kill other buffer")
  "bA" '(my/kill-all-buffers :wk "Kill all buffer")
  "bp" '(my/switch-to-prev-buffer :wk "Prev buffer")
  "bn" '(my/switch-to-next-buffer :wk "Next buffer")
  "bg" '(my/revert-buffer-no-confirm :wk "Revert buffer")
  "bs" '(basic-save-buffer :wk "Save buffer")
  "bS" '(evil-write-all :wk "Save all buffer")
  "c"  '(:wk "Code")
  "cf" '(format-all-buffer :wk "Format buffer")
  "cr" '(lsp-bridge-rename :wk "Rename")
  "e"  '(:ignore t :wk "Error")
  "eb" '(flycheck-buffer :wk "Check current buffer")
  "el" '((lambda()(interactive)(call-interactively 'flycheck-list-errors) (select-window (get-buffer-window "*Flycheck errors*"))) :wk "List errors")
  "en" '(flycheck-next-error :wk "Next error")
  "ep" '(flycheck-previous-error :wk "Previous error")
  "ev" '(flycheck-verify-setup :wk "Verify setup")
  "es" '(flycheck-select-checker :wk "Select checker")
  "f"  '(:ignore t :wk "File")
  "fC" '(my/copy-file :wk "Copy file")
  "fd" '(consult-dir :wk "Open file under dir")
  "fD" '(my/delete-file :wk "Delete file")
  "ff" '(consult-find :wk "Find file")
  "fh" '(find-file :wk "FD find here")
  "fH" '((lambda() (interactive)(find-file (read-file-name "Remote: " "/scp:"))) :wk "Remote")
  "fr" '(recentf-open-files :wk "Recent file")
  "fR" '(my/rename-file :wk "Rename file")
  "g"  '(:ignore t :wk "Git")
  "ga" '(my/git-add-current-file :wk "add-current-file")
  "gb" '(magit-branch-checkout :wk "Checkout branch")
  "gB" '(magit-blame :wk "Magit blame")
  "gc" '(my/git-checkout-current-file :wk "Checkout current file")
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
  "j"  '(:ignore t :wk "Jump")
  "jc" '(avy-goto-char-2 :wk "Jump char")
  "jd" '(dired-jump :wk "Jump dired")
  "jf" 'beginning-of-defun
  "jj" 'scroll-other-window-down
  "jl" '(goto-line :wk "Jump line")
  "jm" '(evil-show-marks :wk "Jump mark")
  "jw" 'avy-goto-word-or-subword-1
  "m"  '(:ignore t :wk "Bookmark")
  "ms" '(bookmark-set :wk "Add bookmark")
  "mr" '(bookmark-rename :wk "Rename bookmark")
  "md" '(bookmark-delete :wk "Delete bookmark")
  "mj" '(bookmark-jump :wk "Jump bookmark")
  "o"  '(:ignore t :wk "Open")
  "oi" '(my/open-init-file :wk "Open init.el")
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
  "s"  '(:ignore t :wk "Search")
  "sb" '(my/consult-line-symbol-at-point :wk "Buffer")
  "sg" '(consult-git-grep :wk "Git")
  "sh" '((lambda() (interactive) (consult-ripgrep default-directory)) :wk "Current dir")
  "sp" '(consult-ripgrep :wk "Project")
  "ss" '(my/consult-ripgrep-at-point :wk "Symbol")
  "st" '(load-theme :wk "Theme")
  "si" '(imenu :wk "Imenu")
  "t"  '(:ignore t :wk "Toggle")
  "td" '(my/toggle-darkroom-mode :wk "Darkroom")
  "tf" '(neotree-toggle :wk "Neotree")
  "tF" '(toggle-frame-fullscreen :wk "Fullscreen")
  "tg" '(my/toggle-golden-ratio :wk "Golden ratio")
  "ti" '(maple-imenu :wk "Imenu")
  "tl" '(toggle-truncate-lines :wk "Truncate line")
  "ts" '(symbol-overlay-mode :wk "Symbol overlay")
  "tt" '(vterm-toggle :wk "Term")
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
  "wt" '(my/toggle-two-split-window :wk "toggle-two-split-window")
  "wz" '(zoom-window-zoom :wk "Zoom window")
  "wl" '(my/split-window-right-and-focus :wk "Split window right")
  "wj" '(my/split-window-below-and-focus :wk "Split window below")
  "w=" '(balance-windows :wk "Balance window")
  "q"  '(:wk "Quit")
  "qq" '(save-buffers-kill-terminal :wk "Quit")
  "qr" '(restart-emacs :wk "Restart"))

(provide 'init-keybindings.el)
;;; init-keybindings.el ends here
