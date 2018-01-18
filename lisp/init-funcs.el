;;; init-funcs.el --- functions used in other package.
;;; Commentary:
;;; Code:

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 64))
      (> (line-number-at-pos (point-max)) 5000)))

(defconst kevin/is-mac
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defun kevin/goto-match-parent (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

(defun kevin/iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

(provide 'init-funcs)
;;; init-funcs.el ends here
