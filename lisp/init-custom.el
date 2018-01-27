;;; init-custom.el --- Initialize custom configurations.
;;; Commentary:
;;; Code:


;; -----------------------------------------------------------------------------
;; const variables
;; -----------------------------------------------------------------------------
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defcustom my-logo (expand-file-name "logo.png" user-emacs-directory)
  "Set Centaur logo.  nil means official logo."
  :type 'string)

;; (defun kevin/goto-match-parent ()
;;   "Go to the matching  if on (){}[], similar to vi style of %."
;;   (interactive "p")
;;   ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
;;   (cond ((looking-at "[\[\(\{]") (evil-jump-item))
;;         ((looking-back "[\]\)\}]" 1) (evil-jump-item))
;;         ;; now, try to succeed from inside of a bracket
;;         ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
;;         ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
;;         (t nil)))

;; -----------------------------------------------------------------------------
;; custom functions
;; -----------------------------------------------------------------------------

;; (if (fboundp 'with-eval-after-load)
;;     (defalias 'after-load 'with-eval-after-load)
;;   (defmacro after-load (feature &rest body)
;;     "After FEATURE is loaded, evaluate BODY."
;;     (declare (indent defun))
;;     `(eval-after-load ,feature
;;        '(progn ,@body))))

(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 1024 1024))
      (> (line-number-at-pos (point-max)) 5000)))


(defun kevin/iterm-focus ()
  "Open iTerm and move cursor to it."
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

(defun kevin/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun kevin/indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
    (progn
      (indent-region (region-beginning) (region-end))
      (message "Indent selected region."))
      (progn
    (indent-buffer)
    (message "Indent buffer.")))))

(provide 'init-custom)
;;; init-custom.el ends here
