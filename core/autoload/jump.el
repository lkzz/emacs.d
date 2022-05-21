;;; jump.el --- autoload functions used in move action. -*- lexical-binding: t -*-
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
(defun my-jump-match-delimiter ()
  "Go to the matching if on (){}[], similar to vi style of %."
  (interactive)
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

;; returns the enclosing character for the character "c"
;;;###autoload
(defun get-enc-char (c) (cond
                         ((string= c "(") ")")
                         ((string= c "[") "]")
                         ((string= c "{") "}")
                         ((string= c ">") "<")
                         ((string= c "<") ">")
                         ((string= c "'") "'")
                         ((string= c "\"") "\"")
                         (t nil)))
(defvar empty-enclose 0)
