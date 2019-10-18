;;; init-ivy.el --- ivy config. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :init
  (kevin/set-leader-keys
    "SPC" 'counsel-M-x
    "s/" 'counsel-rg
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "ss" 'swiper)
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (counsel-grep . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          ;; Ignore order for non-fuzzy searches by default
          (t . ivy--regex-ignore-order)))
  :bind (("C-s" . swiper-isearch)
         ("C-S-s" . swiper-all)
         ("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("C-c t c" . ivy-toggle-calling)
         :map counsel-mode-map
         ([remap swiper] . swiper-isearch)
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
         ([escape] . minibuffer-keyboard-quit)
         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)
         ("C-l" . counsel-down-directory)
         :map swiper-map
         ("M-%" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function nil
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t
        ivy-format-function #'ivy-format-function-arrow
        ivy-count-format "(%d/%d) "
        ;; when non-nil, frame blink in terminal
        swiper-action-recenter nil
        counsel-find-file-at-point t)
  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))

;; Ivy integration for Projectile
(use-package counsel-projectile
  :after (counsel projectile)
  :init (counsel-projectile-mode 1)
  :config
  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil))

;; More friendly interface for ivy
(use-package ivy-rich
  :after (ivy counsel projectile)
  :init
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer (:columns ((ivy-rich-candidate (:width 30))  ; return the candidate itself
                                       (ivy-rich-switch-buffer-size (:width 10 :face warning))  ; return the buffer size
                                       (ivy-rich-switch-buffer-major-mode (:width 20 :face error))          ; return the major mode info
                                       (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                                       (ivy-rich-switch-buffer-path (:width (lambda (x)
                                                                              (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))
                                                                            :face font-lock-comment-face)))
                                      :predicate (lambda (cand) (get-buffer cand)))
                            counsel-M-x  (:columns ((counsel-M-x-transformer (:width 40))  ; thr original transfomer
                                                    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
                            counsel-describe-function (:columns ((counsel-describe-function-transformer (:width 40))  ; the original transformer
                                                                 (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
                            counsel-describe-variable (:columns((counsel-describe-variable-transformer (:width 40))  ; the original transformer
                                                                (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
                            counsel-recentf (:columns ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
                                                       (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))) ; return the last modified time of the file)
                            counsel-bookmark (:columns ((ivy-rich-candidate (:width 20 :face success)) ; return the candidate itself
                                                        (ivy-rich-bookmark-info (:face font-lock-comment-face)))) ; return the last modified time of the file)

                            counsel-find-file (:columns ((ivy-rich-candidate (:width 0.5))
                                                         (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
                            ))
  (setq ivy-rich-parse-remote-buffer nil)
  (ivy-rich-mode 1))

;; For better fuzzy searching
(use-package flx
  :init
  (setq ivy-re-builders-alist
        '((counsel-grep . ivy--regex-plus)
          (counsel-rg   . ivy--regex-plus)
          (counsel-pt   . ivy--regex-plus)
          (swiper       . ivy--regex-plus)
          (t            . ivy--regex-fuzzy))))

(provide 'init-ivy)
;;; init-ivy.el ends here
