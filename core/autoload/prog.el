;;; prog.el --- autoload functions used in prog mode. -*- lexical-binding: t -*-
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
;;; Code:

;;;###autoload
(defun kevin/insert-cc-file-header ()
  "Add ifndef header to an c/c++ header file."
  (interactive)
  (when (string= ".h" (substring (buffer-file-name (current-buffer)) -2))
    (let* ((file-name (buffer-file-name (current-buffer)))
           (fbasename (replace-regexp-in-string ".*/" "" file-name))
           (inc-guard-base (replace-regexp-in-string "[.-]" "_" fbasename))
           (include-guard (string-remove-suffix "_" (concat (upcase inc-guard-base) "_"))))
      (insert "#ifndef " include-guard)
      (newline 1)
      (insert "#define " include-guard)
      (newline 4)
      (insert (format "#endif // %s" include-guard))
      (newline 1)
      (previous-line 3)
      (set-buffer-modified-p nil))))

;;;###autoload
(defun kevin/insert-elisp-file-header ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- insert description here -*- lexical-binding: t -*-\n"
              ";;\n"
              ";; Copyright (C) 2017-2020  Kevin Leung\n"
              ";;\n"
              ";; Author: Kevin Leung <kevin.scnu@gmail.com>\n"
              ";; URL: https://github.com/lkzz/emacs.d\n"
              ";;\n"
              ";; This file is not part of GNU Emacs.\n"
              ";;\n"
              ";;; License: GPLv3\n"
              ";;\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))

;;;###autoload
(defun kevin/goto-definition (&optional arg)
  "Goto definition and add bookmark at line."
  (interactive "P")
  (cl-case major-mode
    (go-mode (call-interactively 'godef-jump))
    (emacs-lisp-mode (elisp-def-mode 1) (call-interactively 'elisp-def))
    (lisp-interaction-mode (elisp-def-mode 1) (call-interactively 'elisp-def))
    (python-mode (call-interactively 'jedi:goto-definition))
    (otherwise
     (counsel-etags-find-tag-at-point)))
  (setq this-command 'kevin/goto-definition))
