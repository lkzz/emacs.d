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
  (general-create-definer kevin/colon-key-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix ";")
  :config
  (kevin/space-key-define
    "d" '(nil :which-key "Delete")
    "d d" '(kevin/delete-delimiter-enclosed-text :wk "delete-enclosed-text")
    "d f" 'delete-frame
    "d w" '(kevin/delete-word :wk "delete-word")
    "f" '(nil :which-key "File")
    "j" '(nil :which-key "Jump")
    "s" '(nil :which-key "Search")
    "t" '(nil :which-key "Toggle")
    "t ;" 'toggle-frame-fullscreen
    "t b" 'toggle-scroll-bar
    "t t" 'toggle-truncate-lines)

  (kevin/colon-key-define
    "e" '(nil :which-key "Emacs")
    "e q" 'save-buffers-kill-terminal
    "e i" '(kevin/open-init-file :wk "open-init-file")
    "f" '(nil :which-key "Font")
    "f =" 'kevin/increase-fontsize
    "f -" 'kevin/decrease-fontsize)

  (kevin/comma-key-define
    "f" '(nil :which-key "Find")
    "f d" 'xref-find-definitions
    "f f" 'find-file-at-point
    "f r" 'xref-find-references
    "f s" 'xref-find-apropos
    "d" '(kevin/delete-word :wk "delete-word")
    "y" '(kevin/copy-word :wk "copy-word")
    "p" '(kevin/cover-word :wk "cover-word")))

(provide 'core-keybind)
;;; core-keybind.el ends here
