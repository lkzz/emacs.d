;;; edit.el --- autoload functions used in edit. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

;;;###autoload
(defun kevin/delete-delimiter-enclosed-text ()
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
(defun kevin/buffer-too-big-p ()
  "Check if buffer size is larger than 1M or has more than 5000 lines."
  (or (> (buffer-size) (* 1024 1024))
      (> (line-number-at-pos (point-max)) 5000)))

;;;###autoload
(defun kevin/open-init-file ()
  "Open emacs init file."
  (interactive)
  (find-file user-init-file))

;;;###autoload
(defun kevin/delete-word ()
  "Delete word under cursor."
  (interactive)
  (let ((end (get-point 'forward-word 1))
        (beg (get-point 'backward-word 1)))
    (delete-region beg end)))

;;;###autoload
(defun kevin/copy-word ()
  "print current word."
  (interactive)
  (kill-new (thing-at-point 'word)))

;;;###autoload
(defun kevin/cover-word ()
  "cover word before point"
  (interactive)
  (kevin/delete-word)
  (evil-paste-before 1))
