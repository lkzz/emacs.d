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

;;;###autoload
(defun kevin/normal-buffer ()
  (or (not buffer-read-only)
      (buffer-file-name)))

;;;###autoload
(defun kevin/switch-to-next-buffer ()
  (interactive)
  (unless (minibufferp)
    (let ((p t) (bn (buffer-name)))
      (switch-to-next-buffer)
      (while (and p (not (kevin/normal-buffer)))
	    (switch-to-next-buffer)
	    (when (string= bn (buffer-name)) (setq p nil))))))

;;;###autoload
(defun kevin/switch-to-prev-buffer ()
  (interactive)
  (unless (minibufferp)
    (let ((p t) (bn (buffer-name)))
      (switch-to-prev-buffer)
      (while (and p (not (kevin/normal-buffer)))
	    (switch-to-prev-buffer)
	    (when (string= bn (buffer-name)) (setq p nil))))))

;;;###autoload
(defun kevin/revert-buffer-no-confirm ()
  "Revert buffer without confirm."
  (interactive)
  (revert-buffer t t))

;;;###autoload
(defun indent-buffer()
  "Indent buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun kevin/indent-region-or-buffer()
  "Indent regex or buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (indent-buffer)
        (message "Indent buffer.")))))

;; Kill all buffers except scratch buffer
;;;###autoload
(defun kevin/kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

;; Kill all buffers except the current one.
;;;###autoload
(defun kevin/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;;###autoload
(defun kevin/create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (erase-buffer)
  (lisp-interaction-mode))

;;;###autoload
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a 'before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;;;###autoload
(defun kevin/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

(use-package reveal-in-osx-finder
  :if is-mac-p
  :commands reveal-in-osx-finder
  :general
  (kevin/space-key-define "b r" 'reveal-in-osx-finder))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :after (projectile)
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  :general
  (kevin/space-key-define
    "b" '(:ignore t :wk "Buffer")
    "b b" 'switch-to-buffer
    "b c" '(kevin/cleanup-buffer :wk "cleanup-buffer")
    "b e" 'eval-buffer
    "b d" 'kill-this-buffer
    "b D" '(kevin/kill-other-buffers :wk "kill-other")
    "b i" '(kevin/indent-region-or-buffer :wk "indent-buffer")
    "b k" 'kill-buffer
    "b l" 'ibuffer-list-buffers
    "b m" '(kevin/kill-all-buffers :wk "kill-all-buffer")
    "b p" '(kevin/switch-to-prev-buffer :wk "prev-buffer")
    "b n" '(kevin/switch-to-next-buffer :wk "next-buffer")
    "b g" '(kevin/revert-buffer-no-confirm :wk "revert-buffer")
    "b s" 'save-buffer
    "b S" '(kevin/create-scratch-buffer :wk "create-scratch-buffer")))

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
      ))
  )

(defun kevin/auto-save-enable ()
  (interactive)
  (run-with-idle-timer 3 t #'kevin/auto-save-buffer)
  (add-hook 'before-save-hook 'font-lock-flush))

(add-hook 'after-init-hook #'kevin/auto-save-enable)

(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

(provide 'init-buffer)
;;; init-buffer.el ends here
