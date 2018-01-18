;;; init-dired.el --- config dired mode
;;; Commentary:
;;; Code:
;; Show directory first

(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t)            ; copy in a split window
  (setq dired-recursive-deletes 'top)   ; "top" means ask cone
  (setq dired-recursive-copies 'always);; "always" means no asking
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))
  ;; Suppress the warning: `ls does not support --dired'.
  (setq dired-use-ls-dired nil)

  ;; Highlights dired buffer like k
  (use-package dired-k
    :bind (:map dired-mode-map ("K" . dired-k))
    :init
    (setq dired-k-padding 1)
    (setq dired-k-human-readable t)
    (setq dired-k-style 'git)
    (setq dired-k-human-readable t))

  ;; Extra Dired functionality
  ;; (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :commands (dired-jump
               dired-jump-other-window
               dired-omit-mode)
    :demand
    :after dired
    :config
    (setq dired-omit-files
      (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

(provide 'init-dired)
;;; init-dired ends here
