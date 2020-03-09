;;; init-ivy.el --- ivy config. -*- lexical-binding: t; -*-
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
;;
;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . swiper-isearch)
         ("C-S-s" . swiper-all)
         ("C-c C-r" . ivy-resume)
         ("C-c t c" . ivy-toggle-calling)
         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)
         ([remap dired] . counsel-dired)
         ([remap recentf-open-files] . counsel-recentf)
         ([remap amx] . counsel-M-x)
         ([remap find-file] . counsel-find-file)
         ([remap switch-to-buffer] . counsel-switch-buffer)
         ("C-x j" . counsel-mark-ring)
         ("C-c C-r" . counsel-recentf)
         ("C-c C-p" . counsel-package)
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
         ("C-c c r" . counsel-rg)
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
        counsel-find-file-at-point t)

  ;; when swiper-action-recenter non-nil, frame blink in terminal
  (if (display-graphic-p)
      (setq swiper-action-recenter t)
    (setq swiper-action-recenter nil))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read)))

;; Ivy integration for Projectile
(use-package counsel-projectile
  :after (counsel projectile)
  :hook (counsel-mode . counsel-projectile-mode)
  :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

;; More friendly interface for ivy
(use-package all-the-icons-ivy-rich
  :if (display-graphic-p)
  :hook (ivy-mode . all-the-icons-ivy-rich-mode))

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :hook (;; Must load after `counsel-projectile'
         (counsel-projectile-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

(use-package prescient
  :init
  (setq prescient-history-length 2000
        prescient-save-file (concat kevin-cache-directory "prescient-items")
        prescient-filter-method '(literal regexp))
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not counsel-grep
               counsel-rg
               counsel-switch-buffer
               ivy-switch-buffer
               swiper
               swiper-multi))
  (setq ivy-prescient-retain-classic-highlighting t
        ivy-prescient-enable-filtering nil
        ivy-prescient-enable-sorting t)
  (ivy-prescient-mode 1))

(provide 'init-ivy)
;;; init-ivy.el ends here
