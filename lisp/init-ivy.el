;;; init-ivy.el --- ivy config
;;; Commentary:
;;; Code:

(use-package counsel
  :ensure t
  :defer t
  :diminish ivy-mode counsel-mode
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
  :init (add-hook 'after-init-hook
                  (lambda ()
                    (ivy-mode 1)
                    (counsel-mode 1)))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")
  ;; Find counsel commands quickly
  (bind-key "<f6>" (lambda ()
                     (interactive)
                     (counsel-M-x "^counsel ")))

  ;; Use faster search tools: ripgrep or the silver search
  (let ((command
         (cond
          ((executable-find "rg")
           "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
          ((executable-find "ag")
           "ag -i --noheading --nocolor --nofilename --numbers '%s' %s"))))
    (setq counsel-grep-base-command command))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Search at point
  ;; "M-j": word-at-point
  ;; "M-n"/"C-w": symbol-at-point
  ;; Refer to https://www.emacswiki.org/emacs/SearchAtPoint#toc8
  ;; and https://github.com/abo-abo/swiper/wiki/FAQ
  ;; (bind-key "C-w" (lambda ()
  ;;                   (interactive)
  ;;                   (insert (format "%s" (with-ivy-window (ivy-thing-at-point)))))
  ;;           ivy-minibuffer-map)


  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :ensure t
    :defer t
    :after flyspell
    :bind (:map flyspell-mode-map
                ("C-;" . flyspell-correct-previous-word-generic)))

  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :ensure t
    :defer t
    :init
    (setq ivy-virtual-abbreviate 'full
          ivy-rich-switch-buffer-align-virtual-buffer t)
    (setq ivy-rich-path-style 'abbrev)

    (ivy-set-display-transformer 'ivy-switch-buffer
                                 'ivy-rich-switch-buffer-transformer)

    (with-eval-after-load 'counsel-projectile
      (ivy-set-display-transformer 'counsel-projectile
                                   'ivy-rich-switch-buffer-transformer)
      (ivy-set-display-transformer 'counsel-projectile-switch-to-buffer
                                   'ivy-rich-switch-buffer-transformer)))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :ensure t
    :defer t
    :init (counsel-projectile-mode 1))

  ;; Display world clock using Ivy
  (use-package counsel-world-clock
    :ensure t
    :defer t
    :bind (:map counsel-mode-map
                ("C-c c c" . counsel-world-clock)))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :ensure t
    :defer t
    :bind (:map counsel-mode-map
                ("C-c c t" . counsel-tramp)))

  ;; Ivy for GNU global
  (use-package counsel-gtags
    :ensure t
    :defer t
    :diminish counsel-gtags-mode
    :bind (:map counsel-gtags-mode-map
                ("M-." . counsel-gtags-find-definition)
                ("M-r" . counsel-gtags-find-reference)
                ("M-s" . counsel-gtags-find-symbol)
                ("M-," . counsel-gtags-go-backward))
    :init
    (setq counsel-gtags-auto-update t)
    (add-hook 'c-mode-hook 'counsel-gtags-mode)
    (add-hook 'c++-mode-hook 'counsel-gtags-mode))
  )

(provide 'init-ivy)
;;; init-ivy.el ends here
