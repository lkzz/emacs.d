;;; init-packages.el --- chinese package config for emacs.
;;; Commentary:
;;; Author: kevin <kevin.scnu@gmail.com>
;;; URL: https://github.com/lkzz/emacs.d
;;; Code:

(use-package youdao-dictionary
  :bind ("C-c y" . 'youdao-dictionary-search-at-point+)
  :config
  (progn
    ;; Enable Cache
    (setq url-automatic-caching t)
    ;; Set file path for saving search history
    (setq youdao-dictionary-search-history-file (concat user-emacs-directory ".youdao"))
    ;; Enable Chinese word segmentation support
    (setq youdao-dictionary-use-chinese-word-segmentation t)))

;; ** 设置拼音输入法
(use-package pyim
  :demand t
  :bind (("M-j" . pyim-convert-code-at-point)
         ;; ("C-;" . Pyim-Delete-Word-from-personal-buffer)
         )

  :config
  (setq default-input-method "pyim")
  ;; 使用 emacs thread 来生成 dcache。
  (setq pyim-dcache-prefer-emacs-thread t)

  ;; 使用全拼
  (setq pyim-default-scheme 'quanpin)

  ;; 设置选词框的绘制方式
  (if (and (display-graphic-p)
           (>= emacs-major-version 26))
      (setq pyim-page-tooltip 'child-frame)
    (setq pyim-page-tooltip 'popup))

  ;; 显示6个候选词。
  (setq pyim-page-length 6)

  ;; emacs 启动时加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda ()
                (pyim-restart-1 t)))

  )
;; 激活 basedict 拼音词库
(use-package pyim-basedict
  :after pyim
  :ensure nil)

(use-package pangu-spacing
  :diminish pangu-spacing-mode
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
          (holiday-float 11 4 4 "感恩节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays
                holiday-other-holidays)))

(provide 'init-chinese)
;;; init-chinese ends here
