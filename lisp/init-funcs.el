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

(provide 'init-funcs)
;;; init-funcs.el ends here
