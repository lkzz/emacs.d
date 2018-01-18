;;; init-evil.el --- setup emacs use evil keybinds
;;; Commentary:
;;; Code:

(require-package 'evil)
(require-package 'goto-chg)
(require-package 'evil-surround)
(require-package 'evil-visualstar)
(require-package 'evil-leader)
(require-package 'evil-numbers)

(require 'evil-visualstar)

(setq evil-mode-line-format 'before)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "f f" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

;; UI
(setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
      evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
      evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
      evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
      evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
      evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

;; prevent esc-key from translating to meta-key in terminal mode
(setq evil-esc-delay 0)
(global-evil-surround-mode 1)

(require 'evil)
(global-evil-leader-mode) ;; enable evil-leader
(evil-mode 1)


;; Scrolling
(defun prelude-evil-scroll-down-other-window ()
  (interactive)
  (scroll-other-window))

(defun prelude-evil-scroll-up-other-window ()
  (interactive)
  (scroll-other-window '-))

(define-key evil-normal-state-map
  (kbd "C-S-d") 'prelude-evil-scroll-down-other-window)

(define-key evil-normal-state-map
  (kbd "C-S-u") 'prelude-evil-scroll-up-other-window)

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
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)


;;
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

(setq evil-shift-width 2)

;;; enable avy with evil-mode
(define-key evil-normal-state-map (kbd "SPC") 'avy-goto-word-1)

;;; snagged from Eric S. Fraga
;;; http://lists.gnu.org/archive/html/emacs-orgmode/2012-05/msg00153.html
(defun prelude-evil-key-bindings-for-org ()
  ;;(message "Defining evil key bindings for org")
  (evil-declare-key 'normal org-mode-map
    "gk" 'outline-up-heading
    "gj" 'outline-next-visible-heading
    "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
    "L" 'org-end-of-line ; smarter behaviour on headlines etc.
    "t" 'org-todo ; mark a TODO item as DONE
    ",c" 'org-cycle
    (kbd "TAB") 'org-cycle
    ",e" 'org-export-dispatch
    ",n" 'outline-next-visible-heading
    ",p" 'outline-previous-visible-heading
    ",t" 'org-set-tags-command
    ",u" 'outline-up-heading
    "$" 'org-end-of-line ; smarter behaviour on headlines etc.
    "^" 'org-beginning-of-line ; ditto
    "-" 'org-ctrl-c-minus ; change bullet style
    "<" 'org-metaleft ; out-dent
    ">" 'org-metaright ; indent
    ))
(prelude-evil-key-bindings-for-org)

(provide 'init-evil)
;;; init-evil ends here
