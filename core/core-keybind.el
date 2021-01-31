;;; core-keybind.el --- core keybinds -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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

;; used as tmux prefix key
(global-unset-key (kbd "C-q"))
(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up   1)))
;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

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
  (dolist (item '((("SPC" . nil) . ("␣" . nil))
                  (("TAB" . nil) . ("↹" . nil))
                  (("RET" . nil) . ("⏎" . nil))
                  (("DEL" . nil) . ("⌫" . nil))
                  (("<up>" . nil) . ("↑" . nil))
                  (("<down>" . nil) . ("↓" . nil))
                  (("<left>" . nil) . ("←" . nil))
                  (("<right>" . nil) . ("→" . nil))
                  (("deletechar" . nil) . ("⌦" . nil))
                  ;; rename winum-select-window-1 entry to 1..9
                  (("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9"))
                  ;; hide winum-select-window-[2-9] entries
                  ((nil . "winum-select-window-[2-9]") . t)))
    (cl-pushnew item which-key-replacement-alist :test #'equal))

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
  :config
  (kevin/space-key-define
    "d" '(nil :wk "delete")
    "d d" '(kevin/delete-delimiter-enclosed-text :wk "delete-enclosed-text")
    "d f" 'delete-frame
    "d w" '(kevin/delete-word :wk "delete-word")
    "f" '(nil :wk "file")
    "f f" 'find-file
    "f i" '(kevin/open-init-file :wk "open-init-file")
    "f r" 'recentf
    "j" '(nil :wk "jump")
    "s" '(nil :wk "search")
    "t" '(nil :wk "toggle")
    "t f" '(toggle-frame-fullscreen :wk "fullscreen")
    "t b" '(toggle-scroll-bar :wk "scroll-bar")
    "t t" '(toggle-truncate-lines :wk "truncate-line"))

  (kevin/comma-key-define
    "f" '(nil :wk "find")
    "f d" 'xref-find-definitions
    "f f" 'find-file-at-point
    "f r" 'xref-find-references
    "f s" 'xref-find-apropos
    "d" '(kevin/delete-word :wk "delete-word")
    "y" '(kevin/copy-word :wk "copy-word")
    "p" '(kevin/cover-word :wk "cover-word")))

(provide 'core-keybind)
;;; core-keybind.el ends here
