;;; init-packages.el --- chinese package config for emacs.-*- lexical-binding: t -*-
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
;;
;;; Code:

(use-package youdao-dictionary
  :commands youdao-dictionary-search-at-point+
  :bind ("C-c y" . 'youdao-dictionary-search-at-point+)
  :config
  ;; Enable Cache
  (setq url-automatic-caching t)
  ;; Set file path for saving search history
  (setq youdao-dictionary-search-history-file (expand-file-name "youdao" kevin-cache-directory))
  ;; Enable Chinese word segmentation support
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;; https://github.com/merrickluo/liberime
(use-package liberime
  :disabled
  :load-path "site-lisp/liberime"
  :init
  (add-hook 'liberime-after-start-hook
            (lambda ()
              (liberime-select-schema "luna_pinyin_simp"))))

;; 设置拼音输入法
(use-package pyim
  :defer t
  :bind (("M-j" . pyim-convert-code-at-point) ; 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文
         ("C-\\" . evil-toggle-input-method))
  :config
  ;; make IME compatible with evil-mode
  ;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-chinese.el#L4
  (defun evil-toggle-input-method ()
    "When input method is on, goto `evil-insert-state'."
    (interactive)
    ;; load IME when needed, less memory footprint
    (unless (featurep 'pyim)
      (require 'pyim))
    ;; some guy don't use evil-mode at all
    (cond
     ((and (boundp 'evil-mode) evil-mode)
      ;; evil-mode
      (cond
       ((eq evil-state 'insert)
        (toggle-input-method))
       (t
        (evil-insert-state)
        (unless current-input-method
          (toggle-input-method))))
      (cond
       (current-input-method
        ;; evil-escape and pyim may conflict
        ;; @see https://github.com/redguardtoo/emacs.d/issues/629
        (evil-escape-mode -1)
        (message "中文输入法开启!"))
       (t
        (evil-escape-mode 1)
        (message "中文输入法关闭!"))))
     (t
      ;; NOT evil-mode
      (toggle-input-method))))

  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :config (pyim-basedict-enable))
  (setq pyim-dcache-directory (expand-file-name "pyim" kevin-cache-directory))
  (setq default-input-method "pyim")
  ;; 使用 emacs thread 来生成 dcache。
  (setq pyim-dcache-prefer-emacs-thread t)
  ;; 使用全拼
  (setq pyim-default-scheme 'rime)
  ;; 显示6个候选词。
  (setq pyim-page-length 6)
  ;; 设置选词框的绘制方式
  (if (display-graphic-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltop 'popup))
  ;; 只能在字符串和 comment 中输入中文
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-program-mode)))

;; Chinese calendar
(use-package cal-china-x
  :after calendar
  :commands cal-china-x-setup
  :init (cal-china-x-setup)
  :config
  (setq calendar-location-name "Chengdu")
  (setq calendar-latitude 30.67)
  (setq calendar-longitude 104.06)
  ;; Holidays
  (setq calendar-mark-holidays-flag t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
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
  :load-path "site-lisp/company-english-helper"
  :bind ("C-c t e" . toggle-company-english-helper))

(provide 'init-chinese)
;;; init-chinese ends here
