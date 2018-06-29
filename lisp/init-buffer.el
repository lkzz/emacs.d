;;; init-buffer.el -- Initialization buffer.
;;; Commentary:
;;; Author: kevin <kevin.scnu@gmail.com>
;;; URL: https://github.com/lkzz/emacs.d
;;; Code:


(defun kevin/normal-buffer ()
  (or (not buffer-read-only)
      (buffer-file-name)))

(defun kevin/switch-to-next-buffer ()
  (interactive)
  (unless (minibufferp)
    (let ((p t) (bn (buffer-name)))
      (switch-to-next-buffer)
      (while (and p (not (kevin/normal-buffer)))
	    (switch-to-next-buffer)
	    (when (string= bn (buffer-name)) (setq p nil))))))

(defun kevin/switch-to-prev-buffer ()
  (interactive)
  (unless (minibufferp)
    (let ((p t) (bn (buffer-name)))
      (switch-to-prev-buffer)
      (while (and p (not (kevin/normal-buffer)))
	    (switch-to-prev-buffer)
	    (when (string= bn (buffer-name)) (setq p nil))))))

(defun kevin/revert-buffer-no-confirm ()
  "Revert buffer without confirm."
  (interactive)
  (revert-buffer t t))

(defun indent-buffer()
  "Indent buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

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
(defun kevin/kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

;; Kill all buffers except the current one.
(defun kevin/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kevin/create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a 'before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun kevin/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

;; buffer related keybindings
(kevin/declare-prefix "b" "buffer")
(kevin/set-leader-keys
  "bb" 'ivy-switch-buffer
  "bc" 'kevin/cleanup-buffer
  "be" 'eval-buffer
  "bd" 'kill-this-buffer
  "bD" #'kevin/kill-other-buffers
  "bf" 'beginning-of-defun
  "bi"  #'kevin/indent-region-or-buffer
  "bk" 'kill-buffer
  "bl" 'ibuffer-list-buffers
  "bm" #'kevin/kill-all-buffers
  "bp" #'kevin/switch-to-prev-buffer
  "bn" #'kevin/switch-to-next-buffer
  "bg" #'kevin/revert-buffer-no-confirm
  "bs" 'save-buffer
  "bS" #'kevin/create-scratch-buffer
  )

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(provide 'init-buffer)
;;; init-buffer.el ends here
