;;; init-packages.el --- chinese package config for emacs.-*- lexical-binding: t -*-
;;; Commentary:
;;; Author: kevin <kevin.scnu@gmail.com>
;;; URL: https://github.com/lkzz/emacs.d
;;; Code:

(use-package youdao-dictionary
  :defer t
  :ensure t
  :bind ("C-c y" . 'youdao-dictionary-search-at-point+)
  :config
  ;; Enable Cache
  (setq url-automatic-caching t)
  ;; Set file path for saving search history
  (setq youdao-dictionary-search-history-file (concat kevin-cache-directory ".youdao"))
  ;; Enable Chinese word segmentation support
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;; ** 设置拼音输入法
(use-package pyim
  :demand t
  :bind (("M-j" . pyim-convert-code-at-point)) ;; 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure t
    :config (pyim-basedict-enable))
  (setq pyim-directory (expand-file-name "pyim/" kevin-cache-directory))
  (setq pyim-dcache-directory (expand-file-name "dcache/" pyim-directory))
  (setq default-input-method "pyim")
  ;; 使用 emacs thread 来生成 dcache。
  (setq pyim-dcache-prefer-emacs-thread t)
  ;; 使用全拼
  (setq pyim-default-scheme 'quanpin)
  ;; 显示6个候选词。
  (setq pyim-page-length 6)
  ;; 设置选词框的绘制方式
  (setq pyim-page-tooltip 'posframe)
  ;; 只能在字符串和 comment 中输入中文
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-program-mode)))

(use-package pangu-spacing
  :defer t
  :diminish pangu-spacing-mode
  :config
  (global-pangu-spacing-mode 1)
  ;; Always insert `real' space in org-mode.
  (add-hook 'org-mode-hook '(lambda ()
                              (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))

;; Chinese calendar
(use-package cal-china-x
  :defer t
  :commands cal-china-x-setup
  :hook (calendar-load . cal-china-x-setup)
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
          (holiday-solar-term "冬至" "冬至")

          ))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays)))

;; https://github.com/manateelazycat/company-english-helper
(use-package company-english-helper
  :ensure nil
  :load-path "vendor/lisp/english/"
  :bind ("C-c t e" . 'toggle-company-english-helper))

;; https://github.com/manateelazycat/insert-translated-name
(use-package insert-translated-name
  :ensure nil
  :load-path "vendor/lisp/english/"
  :bind ("C-c t t" . 'insert-translated-name-insert)
  :config
  (setq insert-translated-name-translate-engine 'youdao)
  (defvar insert-translated-name-camel-style-mode-list
    '(go-mode)))

(provide 'init-chinese)
;;; init-chinese ends here
