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

(defvar kevin-english-font "Monaco"
  "The font name of English.")

(defvar kevin-chinese-font "Songti"
  "The font name for Chinese.")

(defvar kevin-fontsize-pair '(15 . 18)
  "Default font size pair for (english . chinese)")

(defvar kevin-fontsize-pair-list '((5 .  6)
                                   (10 . 12)
                                   (13 . 16)
                                   (15 . 18)
                                   (17 . 20)
                                   (19 . 22)
                                   (20 . 24)
                                   (21 . 26)
                                   (24 . 28)
                                   (26 . 32)
                                   (28 . 34)
                                   (30 . 36)
                                   (34 . 40)
                                   (36 . 44))
  "This list is used to store matching (englis . chinese) font-size.")

(defun kevin-font-exist-p (fontname)
  "Test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname)) nil t)))

(defun kevin/set-font (english chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."
  (if (kevin-font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

  (if (kevin-font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-frame-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese :size (cdr size-pair))))))

(defun kevin/set-step-fontsize (step)
  "Increase/Decrease emacs's font size."
  (let ((scale-steps kevin-fontsize-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq kevin-fontsize-pair
          (or (cadr (member kevin-fontsize-pair scale-steps))
              kevin-fontsize-pair))
    (when kevin-fontsize-pair
      (message "emacs font size set to %.1f" (car kevin-fontsize-pair))
      (kevin/set-font kevin-english-font kevin-chinese-font kevin-fontsize-pair))))

(defun kevin/increase-fontsize ()
  "Decrease emacs's font-size acording kevin-fontsize-pair-list."
  (interactive) (kevin/set-step-fontsize 1))

(defun kevin/decrease-fontsize ()
  "Increase emacs's font-size acording kevin-fontsize-pair-list."
  (interactive) (kevin/set-step-fontsize -1))

;; Setup font size based on font-size-pair
(kevin/set-font kevin-english-font kevin-chinese-font kevin-fontsize-pair)

(provide 'init-font)
