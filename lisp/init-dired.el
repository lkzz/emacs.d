;;; init-dired.el --- config dired mode
;;; Commentary:
;;; Code:
;; Show directory first


(use-package dired
  :defer t
  :config
  (setq dired-dwim-target t)            ; copy in a split window
  (setq dired-recursive-deletes 'top)   ; "top" means ask cone
  (setq dired-recursive-copies 'always);; "always" means no asking
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))  ; was dired-up-directory



;; Highlights dired buffer like k
(use-package dired-k
  :after dired
  :bind (:map dired-mode-map
              ("K" . dired-k)
              ("g" . dired-k))
  :init
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
  :config
  (setq dired-k-padding 1)
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t))

(use-package dired-x
  :after dired)


(provide 'init-dired)
;;; init-dired ends here
