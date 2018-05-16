;;; init-dired.el --- config dired mode
;;; Commentary:
;;; Code:
;; Show directory first

;; Directory operations
(use-package dired
  :ensure nil
  :defer t
  :config
  ;; Show directory first
  ;;  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-dwim-target t)
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
  ;; automatically refresh dired buffer on changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (cond
   (sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    ;; Use GNU ls as `gls' from `coreutils' if available.
    (when (executable-find "gls")
      (setq insert-directory-program "gls")))
   (sys/win32p
    (when (executable-find "ls")
      ;; `dired-quick-sort' needs it
      (setq ls-lisp-use-insert-directory-program t))))

  ;; Extra Dired functionality
  (use-package dired-aux
    :defer t
    :ensure nil)
  (use-package dired-x
    :ensure nil
    :defer t
    :diminish dired-omit-mode
    :init (setq dired-omit-mode t)
    :config
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; ;; bind key: `S'
;; (use-package dired-quick-sort
;;   :ensure t
;;   :defer t
;;   ;;    :if (or (executable-find "gls") (executable-find "ls"))
;;   :init (dired-quick-sort-setup))

;; ;; Highlights dired buffer like k
;; (use-package dired-k
;;   :bind (:map dired-mode-map ("K" . dired-k))
;;   :init
;;   (progn
;;     (setq dired-k-style 'git)
;;     (setq dired-k-padding 1)
;;     (setq dired-k-human-readable t)
;;     ;; always execute dired-k when dired buffer is opened
;;     (add-hook 'dired-initial-position-hook 'dired-k)
;;     ))

;; (use-package diredfl
;;   :after dired
;;   :config
;;   (diredfl-global-mode))

(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :after (dired all-the-icons)
  :hook ((ranger-mode dired-mode) . all-the-icons-dired-mode)
  )

(provide 'init-dired)
;;; init-dired ends here
