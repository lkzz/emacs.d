;;; init-ivy.el --- ivy config
;;; Commentary:
;;; Code:

(use-package counsel
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

  ;; Enhance M-x
  (use-package smex)

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :bind (:map ivy-minibuffer-map
                ("M-o" . ivy-dispatching-done-hydra)))

  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :after flyspell
    :bind (:map flyspell-mode-map
                ("C-;" . flyspell-correct-previous-word-generic)))

  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
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
    :init (counsel-projectile-mode 1))

  ;; Display world clock using Ivy
  (use-package counsel-world-clock
    :bind (:map counsel-mode-map
                ("C-c c c" . counsel-world-clock)))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :bind (:map counsel-mode-map
                ("C-c c t" . counsel-tramp)))

  ;; Ivy for GNU global
  (use-package counsel-gtags
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

  ;; Support pinyin in Ivy
  ;; Input prefix '!' to match pinyin
  ;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
  ;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
  (use-package pinyinlib
    :commands pinyinlib-build-regexp-string
    :init
    (defun re-builder-pinyin (str)
      (or (pinyin-to-utf8 str)
          (ivy--regex-plus str)
          (ivy--regex-ignore-order str)))

    (setq ivy-re-builders-alist
          '((t . re-builder-pinyin)))

    (defun my-pinyinlib-build-regexp-string (str)
      (cond ((equal str ".*")
             ".*")
            (t
             (pinyinlib-build-regexp-string str t))))

    (defun my-pinyin-regexp-helper (str)
      (cond ((equal str " ")
             ".*")
            ((equal str "")
             nil)
            (t
             str)))

    (defun pinyin-to-utf8 (str)
      (cond ((equal 0 (length str))
             nil)
            ((equal (substring str 0 1) "!")
             (mapconcat 'my-pinyinlib-build-regexp-string
                        (remove nil (mapcar 'my-pinyin-regexp-helper
                                            (split-string
                                             (replace-regexp-in-string "!" "" str ) "")))
                        ""))
            (t
             nil))))
  )
;; (add-hook 'after-init-hook 'ivy-mode)
;; (after-load 'ivy
;;   (setq-default ivy-use-virtual-buffers t
;;                 ivy-virtual-abbreviate 'fullpath
;;                 ivy-count-format ""
;;                 projectile-completion-system 'ivy
;;                 ivy-magic-tilde nil
;;                 ivy-dynamic-exhibit-delay-ms 150
;;                 ivy-initial-inputs-alist
;;                 '((man . "^")
;;                   (woman . "^")))

;;     ;; IDO-style directory navigation
;;     (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
;;     (dolist (k '("C-j" "C-RET"))
;;       (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))

;;     (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
;;     (global-set-key (kbd "C-x C-r") 'counsel-recentf)
;;     (global-set-key (kbd "C-s") 'swiper)
;;     (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;     (global-set-key (kbd "M-x") 'counsel-M-x)
;;     (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;     (global-set-key (kbd "C-h f") 'counsel-describe-function)
;;     (global-set-key (kbd "C-h v") 'counsel-describe-variable)
;;     (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;     (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;     (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;     (global-set-key (kbd "C-c g") 'counsel-git)
;;     (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;     (global-set-key (kbd "C-c /") 'counsel-ag)
;;     (global-set-key (kbd "C-x l") 'counsel-locate)
;;     (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;     (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;;     (when (maybe-require-package 'diminish)
;;       (diminish 'ivy-mode)))

;;   (defun sanityinc/enable-ivy-flx-matching ()
;;     "Make `ivy' matching work more like IDO."
;;     (interactive)
;;     (require-package 'flx)
;;     (setq-default ivy-re-builders-alist
;;                   '((t . ivy--regex-fuzzy))))

;; (when (maybe-require-package 'ivy-historian)
;;   (add-hook 'after-init-hook (lambda () (ivy-historian-mode t))))

;; (when (maybe-require-package 'counsel)
;;   (setq-default counsel-mode-override-describe-bindings t)
;;   (when (maybe-require-package 'diminish)
;;     (after-load 'counsel
;;       (diminish 'counsel-mode)))
;;   (add-hook 'after-init-hook 'counsel-mode)

;;   (when (maybe-require-package 'projectile)
;;     (let ((search-function
;;            (cond
;;             ((executable-find "rg") 'counsel-rg)
;;             ((executable-find "ag") 'counsel-ag)
;;             ((executable-find "pt") 'counsel-pt)
;;             ((executable-find "ack") 'counsel-ack))))
;;       (when search-function
;;         (defun sanityinc/counsel-search-project (initial-input &optional use-current-dir)
;;           "Search using `counsel-ag' from the project root for INITIAL-INPUT.
;; If there is no project root, or if the prefix argumentq
;; USE-CURRENT-DIR is set, then search from the current directory
;; instead."
;;           (interactive (list (thing-at-point 'symbol)
;;                              current-prefix-arg))
;;           (let ((current-prefix-arg)
;;                 (dir (if use-current-dir
;;                          default-directory
;;                        (condition-case err
;;                            (projectile-project-root)
;;                          (error default-directory)))))
;;             (funcall search-function initial-input dir)))))
;;     (global-set-key (kbd "s-/") 'sanityinc/counsel-search-project)))

;; (after-load 'ivy
;;   (defun sanityinc/swiper-at-point (sym)
;;     "Use `swiper' to search for the symbol at point."
;;     (interactive (list (thing-at-point 'symbol)))
;;     (swiper sym)
;; (define-key ivy-mode-map (kbd "C-s") 'sanityinc/swiper-at-point)))


;; (when (maybe-require-package 'ivy-xref)
;;   (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))


(provide 'init-ivy)
;;; init-ivy.el ends here
