;;; init-dired.el --- config dired mode. -*- lexical-binding: t; -*-
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

;; Directory operations
(use-package dired
  :ensure nil
  :config
  ;; Show directory first
  ;;  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-dwim-target t)
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
  ;; automatically refresh dired buffer on changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (cond
   (kevin-mac-p
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    ;; Use GNU ls as `gls' from `coreutils' if available.
    (when (executable-find "gls")
      (setq insert-directory-program "gls")))
   (kevin-windows-p
    (when (executable-find "ls")
      ;; `dired-quick-sort' needs it
      (setq ls-lisp-use-insert-directory-program t))))

  ;; Extra Dired functionality
  (use-package dired-aux  :ensure nil)
  (use-package dired-x
    :ensure nil
    :diminish dired-omit-mode
    :init (setq dired-omit-mode t)
    :config
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; ;; bind key: `S'
;; (use-package dired-quick-sort
;;   :if (or (executable-find "gls") (executable-find "ls"))
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
  :hook ((dired-mode . all-the-icons-dired-mode)
	     (ranger-mode . all-the-icons-dired-mode)))

(provide 'init-dired)
;;; init-dired ends here
