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
  (interactive)
  (let ((autosave-buffer-list))
    (ignore-errors
      (save-excursion
        (dolist (buf (buffer-list))
          (set-buffer buf)
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
                 (evil-normal-state-p))
            (push (buffer-name) autosave-buffer-list)
            (basic-save-buffer))))
      (cond
       ((= (length autosave-buffer-list) 1)
        (message "# Auto saved %s" (car autosave-buffer-list)))
       ((> (length autosave-buffer-list) 1)
        (message "# Auto saved %d files: %s"
                 (length autosave-buffer-list)
                 (mapconcat 'identity autosave-buffer-list ", "))))
      )))

(defun kevin/auto-save-enable ()
  (interactive)
  (run-with-idle-timer 1 t #'kevin/auto-save-buffer)
  (add-hook 'before-save-hook 'font-lock-flush))

(add-hook 'after-init-hook #'kevin/auto-save-enable)

(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

(provide 'init-buffer)
;;; init-buffer.el ends here
