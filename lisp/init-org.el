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
  :defer t
  :bind (("C-c B" . org-switchb)
         :map org-mode-map
         ("C-c l" . org-store-link))
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (org-indent-mode 1)
                             (diminish 'org-indent-mode)))
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ee6363" :weight bold))
          ("DOING" . (:foreground "#3a81c3" :weight bold))
          ("HANGUP" . (:foreground "red" :weight bold))
          ("DONE" . (:foreground "#7ccd7c" :weight bold))
          ("CANCEL"  . (:foreground "yellow" :weight bold))))
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (add-to-list 'org-export-backends 'md)

  (setq org-clock-string "计时:")
  (setq org-closed-string "已关闭:")
  (setq org-deadline-string "最后期限:")
  (setq org-scheduled-string "计划任务:")
  ;; Fast TODO Selection
  (setq org-use-fast-todo-selection t)
  ;; record timestamp when a task moves to the DONE state
  (setq org-log-done 'time)
  ;; Log time when rescheduling an entry.
  (setq org-log-reschedule 'time)
  (setq org-log-redeadline 'time)
  (setq org-log-into-drawer t)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Don't clock out when moving task to a done state
  (setq org-clock-out-when-done nil)
  ;; Save the running clock and all clock history when exiting Emacs,load it on startup
  (setq org-clock-persist t)

  ;; FIXME org-agenda-execute-calendar-command uses deprecated list-calendar-holidays
  (unless (fboundp 'list-calendar-holidays)
    (defalias 'list-calendar-holidays 'calendar-list-holidays))

  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)
                                 (ruby . t)))

  (use-package org-bullets
    :ensure t
    :init
    (setq org-bullets-bullet-list
          '("✡" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))
    (add-hook 'org-mode-hook #'org-bullets-mode))

  ;; Presentation
  (use-package org-tree-slide
    :config
    (add-hook 'org-tree-slide-play-hook
              (lambda ()
                (text-scale-set 4)
                (org-display-inline-images)
                (read-only-mode 1)))
    (add-hook 'org-tree-slide-stop-hook
              (lambda ()
                (text-scale-set 0)
                (org-remove-inline-images)
                (read-only-mode -1))))

  ;; Pomodoro
  (use-package org-pomodoro
    :init (with-eval-after-load 'org-agenda
            (bind-key "P" 'org-pomodoro org-agenda-mode-map)))

  ;; Visually summarize progress
  (use-package org-dashboard
    :ensure t)
  )

(use-package org-archive
  :after org-agenda
  :ensure nil
  :config
  ;; 使用 org-archive-subtree 时，原来的 header 层级容易被打乱，而且容易
  ;; 因为保存不及时而导致 archive 文件内容丢失， 所以这个命令适合每月的
  ;; 大归档, 日常情况下，使用 ARCHIVE TAG 来隐藏已经完成的任务，安全又方便。
  ;; (setq org-archive-default-command 'org-archive-subtree)
  (setq org-archive-default-command 'org-archive-set-tag)
  )

(use-package org-agenda
  :ensure nil
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
  (setq org-agenda-files '("~/Workspace/org"))
  ;; Set the agenda view to show the tasks on day/week/month/year
  (setq org-agenda-span 'week)
  ;; only keep agenda window,delete all other window
  (setq org-agenda-window-setup 'only-window)
  ;; (setq org-agenda-todo-ignore-scheduled t)
  ;; (setq org-agenda-todo-ignore-deadlines t)
  ;; (setq org-agenda-skip-deadline-if-done t)
  ;; (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-todo-list-sublevels t)
  ;; format 9:30-->09:30
  (setq org-agenda-time-leading-zero nil)
  (setq org-agenda-format-date "%Y-%m-%d %a----------------------------------------------------------------")
  ;; Custom commands for the agenda -- start with a clean slate.
  (setq org-agenda-custom-commands nil)
  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-scheduled-leaders
        '("计划任务 " "应在 %02d 天前开始 "))
  (setq org-agenda-deadline-leaders
        '("过期任务 " "将在 %02d 天后到期 " "已过期 %02d 天 "))
  )

(provide 'init-org)

;;; init-org.el ends here
