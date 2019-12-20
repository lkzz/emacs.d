;;; init-org.el --- Initialize org configurations. -*- lexical-binding: t; -*-
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

(use-package org
  :bind (("C-c b" . org-switchb)
         :map org-mode-map
         ("C-c l" . org-store-link))
  :config
  (add-to-list 'org-export-backends 'md)
  (setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)"))
        org-todo-keyword-faces '(("TODO" . (:foreground "#ee6363" :weight bold))
                                 ("DOING" . (:foreground "#3a81c3" :weight bold))
                                 ("HANGUP" . (:foreground "red" :weight bold))
                                 ("DONE" . (:foreground "#7ccd7c" :weight bold))
                                 ("CANCEL"  . (:foreground "yellow" :weight bold)))
        org-log-done 'time
        org-src-fontify-natively t
        org-clock-string "计时:"
        org-closed-string "已关闭:"
        org-deadline-string "最后期限:"
        org-scheduled-string "计划任务:"
        ;; Fast TODO Selection
        org-use-fast-todo-selection t
        ;; record timestamp when a task moves to the DONE state
        org-log-done 'time
        ;; Log time when rescheduling an entry.
        org-log-reschedule 'time
        org-log-redeadline 'time
        org-log-into-drawer t
        ;; Resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;; Don't clock out when moving task to a done state
        org-clock-out-when-done nil
        ;; Save the running clock and all clock history when exiting Emacs,load it on startup
        org-clock-persist t
        org-confirm-babel-evaluate nil
        org-agenda-inhibit-startup t ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        org-pretty-entities t)


  ;; FIXME org-agenda-execute-calendar-command uses deprecated list-calendar-holidays
  (unless (fboundp 'list-calendar-holidays)
    (defalias 'list-calendar-holidays 'calendar-list-holidays))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)
                                 (ruby . t)))
  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :init
  (setq org-bullets-bullet-list '("✡" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇")))

;; Presentation
(use-package org-tree-slide
  :after org-mode
  :config
  (add-hook 'org-tree-slide-play-hook (lambda ()
                                        (text-scale-set 4)
                                        (org-display-inline-images)
                                        (read-only-mode 1)))
  (add-hook 'org-tree-slide-stop-hook (lambda ()
                                        (text-scale-set 0)
                                        (org-remove-inline-images)
                                        (read-only-mode -1))))

;; Pomodoro
(use-package org-pomodoro
  :after org-mode
  :init (with-eval-after-load 'org-agenda
          (bind-key "P" 'org-pomodoro org-agenda-mode-map)))

;; Visually summarize progress
(use-package org-dashboard
  :after org-mode)

(use-package org-archive
  :straight (:type built-in)
  :after (org-mode org-agenda)
  :config
  ;; 使用 org-archive-subtree 时，原来的 header 层级容易被打乱，而且容易
  ;; 因为保存不及时而导致 archive 文件内容丢失， 所以这个命令适合每月的
  ;; 大归档, 日常情况下，使用 ARCHIVE TAG 来隐藏已经完成的任务，安全又方便。
  ;; (setq org-archive-default-command 'org-archive-subtree)
  (setq org-archive-default-command 'org-archive-set-tag))

(use-package org-agenda
  :straight (:type built-in)
  :after org-mode
  :bind (("C-c a" . org-agenda)
         :map org-agenda-mode-map
         ("g" . org-agenda-redo-all)
         ("i" . (lambda () (interactive) (org-capture nil "s")))
         ("A" . org-agenda-archive-default-with-confirmation)
         ("J" . counsel-org-agenda-headlines)
         ("h" . ignore)
         ("y" . ignore)
         ("a" . ignore))
  :config
  (setq org-agenda-files '("~/Workspace/org")
        ;; Set the agenda view to show the tasks on day/week/month/year
        org-agenda-span 'week
        ;; only keep agenda window,delete all other window
        org-agenda-window-setup 'only-window
        org-agenda-todo-ignore-scheduled t
        org-agenda-todo-ignore-deadlines t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-todo-list-sublevels t
        ;; format 9:30-->09:30
        org-agenda-time-leading-zero nil
        org-agenda-format-date "%Y-%m-%d %a----------------------------------------------------------------"
        ;; Custom commands for the agenda -- start with a clean slate.
        org-agenda-custom-commands nil
        ;; Do not dim blocked tasks
        org-agenda-dim-blocked-tasks nil
        ;; Compact the block agenda view
        org-agenda-compact-blocks t
        org-agenda-scheduled-leaders '("计划任务 " "应在 %02d 天前开始 ")
        org-agenda-deadline-leaders '("过期任务 " "将在 %02d 天后到期 " "已过期 %02d 天 ")))

(provide 'init-org)

;;; init-org.el ends here
