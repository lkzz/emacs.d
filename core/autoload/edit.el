;;; edit.el --- autoload functions used in edit. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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
(defun get-enc-char (c)
  (cond
   ((string= c "(") ")")
   ((string= c "[") "]")
   ((string= c "{") "}")
   ((string= c ">") "<")
   ((string= c "<") ">")
   ((string= c "'") "'")
   ((string= c "\"") "\"")
   (t nil)))
(defvar empty-enclose 0)

;;;###autoload
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

;;;###autoload
(defun my/delete-delimiter-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (setq empty-enclose 0)
  (save-excursion
    (let (p1 p2 orig)
      (setq orig (point))
      (setq p1 (point))
      (setq p2 (point))
      (setq find 0)
      (setq mychar (thing-at-point 'char))
      (if (-contains? '("(" "[" "{" "<" "'" "\"") mychar)
          (progn
            (setq left_encloser (thing-at-point 'char))
            (backward-char -1)
            (if (string-equal (thing-at-point 'char) (get-enc-char left_encloser))
                (progn
                  (backward-char -1)
                  (setq p2 (point))
                  (setq find 1)
                  (setq empty-enclose 1)))))
      (while (eq find 0)
        (skip-chars-backward "^({[<>\"'")
        (setq p1 (point))
        (backward-char 1)
        (setq left_encloser (thing-at-point 'char))
        (goto-char orig)
        (while (and (not (eobp)) (eq find 0))
          (backward-char -1)
          (skip-chars-forward "^)}]<>\"'")
          (setq right_encloser (thing-at-point 'char))
          (if (string-equal right_encloser (get-enc-char left_encloser))
              (progn
                (setq p2 (point))
                (setq find 1))))
        (goto-char p1)
        (backward-char 1))
      (delete-region p1 p2)))
  (if (eq empty-enclose 0)
      (backward-char 1)))

;;;###autoload
(defun my/buffer-too-big-p ()
  "Check if buffer size is larger than 1M or has more than 5000 lines."
  (or (> (buffer-size) (* 1024 1024))
      (> (line-number-at-pos (point-max)) 5000)))

;;;###autoload
(defun my/open-init-file ()
  "Open emacs init file."
  (interactive)
  (find-file user-init-file))

;;;###autoload
(defun my/delete-word ()
  "Delete word under cursor."
  (interactive)
  (let ((end (get-point 'forward-word 1))
        (beg (get-point 'backward-word 1)))
    (delete-region beg end)))

;;;###autoload
(defun my/copy-word ()
  "print current word."
  (interactive)
  (kill-new (thing-at-point 'word)))

;;;###autoload
(defun my/cover-word ()
  "cover word before point"
  (interactive)
  (my/delete-word)
  (evil-paste-before 1))

;;;###autoload
(defun my/insert-cc-file-header ()
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
(defun my/insert-elisp-file-header ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- insert description here -*- lexical-binding: t -*-\n"
              ";;\n"
              ";; Copyright (C) 2017-2022 kevin.scnu@gmail.com\n"
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
