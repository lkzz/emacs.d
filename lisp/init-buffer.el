;;; init-buffer.el -- Initialization buffer. -*- lexical-binding: t; -*-
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

(use-package reveal-in-osx-finder
  :if is-mac-p
  :commands reveal-in-osx-finder)

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :after projectile
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(defun kevin/auto-save-buffer()
  (ignore-errors
    (save-excursion
      (when (and
             ;; filename is not empty
             (buffer-file-name)
             ;; buffer is modified
             (buffer-modified-p)
             ;; smerge mode is not active
             (not smerge-mode)
             ;; yassnippet is not active
             (or (not (boundp 'yas--active-snippets))
                 (not yas--active-snippets))
             ;; company is not active
             (or (not (boundp 'company-candidates))
                 (not company-candidates))
             ;; evil normal state
             (evil-normal-state-p)
             )
        (basic-save-buffer)
        (message "# saved %s" buffer-file-name))
      )))

(defun kevin/auto-save-enable ()
  (interactive)
  (run-with-idle-timer 3 t #'kevin/auto-save-buffer)
  (add-hook 'before-save-hook 'font-lock-flush))

(add-hook 'after-init-hook #'kevin/auto-save-enable)

(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

(provide 'init-buffer)
;;; init-buffer.el ends here
