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

(defvar kevin/cache-directory (expand-file-name "cache/" user-emacs-directory)
  "Emacs cache directory.")

;; -----------------------------------------------------------------------------
;; custom functions
;; -----------------------------------------------------------------------------

(defun kevin/goto-match-parent ()
  "Go to the matching  if on (){}[], similar to vi style of %."
  (interactive)
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

(defun kevin/buffer-too-big-p ()
  "Check if buffer size is larger than 1M or has more than 5000 lines."
  (or (> (buffer-size) (* 1024 1024))
      (> (line-number-at-pos (point-max)) 5000)))

(defun kevin/open-iterm ()
  "Open iTerm and focus on it."
  (interactive)
  (do-applescript
   "do shell script \"open -a iTerm\"\n"))

(defun kevin/open-wechat ()
  "Open WeChat and focus on it."
  (interactive)
  (do-applescript
   "do shell script \"open -a WeChat\"\n"))

(defun kevin/open-youdao ()
  "Open youdao dictionary and focus on it."
  (interactive)
  (do-applescript
   "do shell script \"open -a 有道词典\"\n"))

(defun kevin/open-chrome ()
  "Open chrome dictionary and focus on it."
  (interactive)
  (do-applescript
   "do shell script \"open -a Google Chrome\"\n"))


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

(defun kevin/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

;; Kill all buffers except scratch buffer
(defun kevin/kill-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

(defun kevin/create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun kevin/make-frame ()
  "New a frame,and erase buffer."
  (interactive)
  (make-frame)
  (kevin/create-scratch-buffer))

(defun kevin/go-enable-gometalinter ()
  "Enable `flycheck-gometalinter' and disable overlapping `flycheck' linters."
  (setq flycheck-disabled-checkers '(go-gofmt
                                     go-golint
                                     go-vet
                                     go-build
                                     go-test
                                     go-errcheck))
  (flycheck-gometalinter-setup))


(provide 'init-custom)
;;; init-custom.el ends here
