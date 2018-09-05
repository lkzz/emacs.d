;;; init-ivy.el --- ivy config. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(use-package counsel
  :defer t
  :diminish ivy-mode counsel-mode
  :init
  (progn
    (kevin/set-leader-keys
     "SPC" 'counsel-M-x
     "/" 'counsel-rg
     "ff" 'counsel-find-file
     "fr" 'counsel-recentf
     "ss" 'swiper))
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-all)
         ("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf)
         ("C-x j" . counsel-mark-ring)
         ("C-c C-p" . counsel-package)
         ("C-c c L" . counsel-find-library)
         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-fzf)
         ("C-c c g" . counsel-grep)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-load-library)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)
         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)
         :map swiper-map
         ("M-%" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (after-init . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 20)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")
  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))

;; Ivy integration for Projectile
(use-package counsel-projectile
  :after (counsel projectile)
  :init (counsel-projectile-mode 1))

;; More friendly interface for ivy
(use-package ivy-rich
  :ensure t
  :after (ivy counsel projectile)
  :config
  ;; replace “/home/username” with “~”
  (setq ivy-rich-path-style 'abbrev)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-rich--display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30))  ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 10 :face warning))  ; return the buffer size
            (ivy-rich-switch-buffer-major-mode (:width 20 :face error))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path (:width (lambda (x)
                                                   (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))
                                                 :face font-lock-comment-face)))
           :predicate (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))  ; thr original transfomer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))) ; return the last modified time of the file)
          counsel-bookmark
          (:columns
           ((ivy-rich-candidate (:width 20 :face success)) ; return the candidate itself
            (ivy-rich-bookmark-info (:face font-lock-comment-face)))) ; return the last modified time of the file)
          )
        )

  (ivy-rich-mode 1))

(provide 'init-ivy)
;;; init-ivy.el ends here
