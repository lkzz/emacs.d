;;; init-font.el ---  字体设置. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019  Kevin Leung
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

(defvar kevin-english-font "DejaVu Sans Mono"
  "The font name of English.")

(defvar kevin-chinese-font "STKaiti"
  "The font name for Chinese.")

(defvar kevin-fontsize-pair '(12.5 . 14.5)
  "Default font size pair for (english . chinese)")

(defvar kevin-fontsize-pair-list '((9    . 10.5)
                                   (10   . 12.0)
                                   (11.5 . 14.0)
                                   (12.5 . 14.5)
                                   (14   . 16.5)
                                   (15   . 18.0)
                                   (16   . 20.0)
                                   (18   . 22.5))

  "This list is used to store matching (englis . chinese) font-size.")

(defun kevin-font-exist-p (fontname)
  "Test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname)) nil t)))

(defun kevin/set-pair-font (english chinese size-pair)
  "Setup emacs English and Chinese font on mac."
  (if (kevin-font-exist-p english)
      (let* ((fontname (car (x-list-fonts english nil nil 1))))
        (if fontname
            (set-face-attribute 'default nil
                                :font (font-spec :name fontname
                                                 :weight 'normal
                                                 :slant 'normal
                                                 :size (cdr size-pair)))
          (message (format "english font:%s not exist" english))))
    (message (format "english font:%s not exist" english)))

  (if (kevin-font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset (font-spec :family chinese
                                             :size (cdr size-pair)
                                             :weight 'normal
                                             :slant 'normal)))
    (message (format "chinese font:%s not exist" chinese))))

(defun kevin/set-step-fontsize (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps kevin-fontsize-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq kevin-fontsize-pair
          (or (cadr (member kevin-fontsize-pair scale-steps))
              kevin-fontsize-pair))
    (if kevin-fontsize-pair
        (progn
          (message "Font(%s,%s) fontsize set to %.1f" kevin-english-font kevin-chinese-font (car kevin-fontsize-pair))
          (kevin/set-pair-font kevin-english-font kevin-chinese-font kevin-fontsize-pair))
      (message "Font(%s,%s) fontsize is already the max/min pair" kevin-english-font kevin-chinese-font))))

(defun kevin/increase-fontsize ()
  "Decrease emacs's font-size acording kevin-fontsize-pair-list."
  (interactive)
  (kevin/set-step-fontsize 1))

(defun kevin/decrease-fontsize ()
  "Increase emacs's font-size acording kevin-fontsize-pair-list."
  (interactive)
  (kevin/set-step-fontsize -1))

;; Setup font size based on font-size-pair
(when (display-graphic-p)
  (kevin/set-pair-font kevin-english-font kevin-chinese-font kevin-fontsize-pair))

(provide 'init-font)
