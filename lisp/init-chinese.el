;;; packages.el --- kevin-chinese-layer layer packages file for Spacemacs.
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;;; Code:

(use-package youdao-dictionary
  :ensure t
  :config
  (progn
    ;; Enable Cache
    (setq url-automatic-caching t
          ;; Set file path for saving search history
          youdao-dictionary-search-history-file
          (concat user-emacs-directory ".youdao")
          ;; Enable Chinese word segmentation support
          youdao-dictionary-use-chinese-word-segmentation t)))

(use-package pyim
  :init
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))
  (progn
    (setq pyim-directory (expand-file-name "pyim/" user-emacs-directory))
    (setq pyim-dcache-directory (expand-file-name "dcache/" pyim-directory))
    (setq default-input-method "pyim")
    (setq pyim-default-scheme 'quanpin)
    (setq pyim-page-tooltip t)
    ;; 选词框显示6个候选词
    (setq pyim-page-length 6)
    ;; 让 Emacs 启动时自动加载 pyim 词库
    (add-hook 'emacs-startup-hook
              #'(lambda () (pyim-restart-1 t)))
    ;; 如果当前的 mode 衍生自 prog-mode，那么仅仅在字符串和 comment 中开启中文输入模式
    (setq-default pyim-english-input-switch-functions
                  '(pyim-probe-program-mode))
    (evilified-state-evilify pyim-dm-mode pyim-dm-mode-map)))

(use-package pangu-spacing
  :defer t
  :init (progn (global-pangu-spacing-mode 1)
               ;; Always insert `real' space in org-mode.
               (add-hook 'org-mode-hook
                         '(lambda ()
                            (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))))

;; Chinese calendar
(use-package cal-china-x
  :commands cal-china-x-setup
  :init (add-hook 'calendar-load-hook #'cal-china-x-setup)
  :config
  ;; `S' can show the time of sunrise and sunset on Calendar
  (setq calendar-location-name "Chengdu"
        calendar-latitude 30.67
        calendar-longitude 104.06)

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
          (holiday-float 11 4 4 "感恩节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays)))

(provide 'init-chinese)
;;; init-chinese ends here
