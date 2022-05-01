;;; init-packages.el --- chinese package config for emacs.-*- lexical-binding: t -*-
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
;;
;;; Code:

(use-package youdao-dictionary
  :bind (("C-c y" . kevin/youdao-dictionary-search-at-point))
  :init (setq url-automatic-caching t
              youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词
  (defun kevin/youdao-dictionary-search-at-point ()
    "Search word at point and display result with `posframe' or `popup'"
    (interactive)
    (if (display-graphic-p)
        (youdao-dictionary-search-at-point-posframe)
      (youdao-dictionary-search-at-point+))))

(use-package rime
  :custom
  (rime-librime-root "~/Dropbox/librime/dist")
  (rime-user-data-dir "~/Dropbox/RimeSync")
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  :custom-face
  (rime-code-face ((t (:foreground "#ee6363"))))
  (rime-candidate-num-face ((t (:foreground "#ee6363"))))
  :config
  (setq rime-posframe-properties
        (list :font "STKaiti-16"
              :internal-border-width 10)))

;; Chinese calendar
(use-package cal-china-x
  :after calendar
  :commands cal-china-x-setup
  :init (cal-china-x-setup)
  :config
  (setq calendar-location-name "Chengdu"
        calendar-latitude 30.67
        calendar-longitude 104.06
        calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays
        '((holiday-lunar 1 15 "元宵节")
          (holiday-lunar 7 7 "七夕节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 12 "植树节")
          (holiday-fixed 5 4 "青年节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-fixed 9 10 "教师节")))
  (setq holiday-other-holidays
        '((holiday-fixed 2 14 "情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 12 25 "圣诞节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-float 11 4 4 "感恩节")
          ;; 农历节日
          (holiday-solar-term "清明" "清明节")
          (holiday-solar-term "小寒" "小寒")
          (holiday-solar-term "大寒" "大寒")
          (holiday-solar-term "立春" "立春")
          (holiday-solar-term "雨水" "雨水")
          (holiday-solar-term "惊蛰" "惊蛰")
          (holiday-solar-term "春分" "春分")
          (holiday-solar-term "谷雨" "谷雨")
          (holiday-solar-term "立夏" "立夏")
          (holiday-solar-term "小满" "小满")
          (holiday-solar-term "芒种" "芒种")
          (holiday-solar-term "夏至" "夏至")
          (holiday-solar-term "小暑" "小暑")
          (holiday-solar-term "大暑" "大暑")
          (holiday-solar-term "立秋" "立秋")
          (holiday-solar-term "处暑" "处暑")
          (holiday-solar-term "白露" "白露")
          (holiday-solar-term "秋分" "秋分")
          (holiday-solar-term "寒露" "寒露")
          (holiday-solar-term "霜降" "霜降")
          (holiday-solar-term "立冬" "立冬")
          (holiday-solar-term "小雪" "小雪")
          (holiday-solar-term "大雪" "大雪")
          (holiday-solar-term "冬至" "冬至")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays)))

;; https://github.com/manateelazycat/company-english-helper
(use-package company-english-helper
  :straight (company-english-helper :host github :repo "manateelazycat/company-english-helper")
  :bind (("C-c t e" . toggle-company-english-helper)))

(provide 'init-chinese)
;;; init-chinese ends here
