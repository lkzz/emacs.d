;;; init-org.el --- Initialize org configurations.
;;; Commentary:
;;; Code:


;; ;;; snagged from Eric S. Fraga
;; ;;; http://lists.gnu.org/archive/html/emacs-orgmode/2012-05/msg00153.html
;; (defun prelude-evil-key-bindings-for-org ()
;;   ;;(message "Defining evil key bindings for org")
;;   (evil-declare-key 'normal org-mode-map
;;     "gk" 'outline-up-heading
;;     "gj" 'outline-next-visible-heading
;;     "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
;;     "L" 'org-end-of-line ; smarter behaviour on headlines etc.
;;     "t" 'org-todo ; mark a TODO item as DONE
;;     ",c" 'org-cycle
;;     (kbd "TAB") 'org-cycle
;;     ",e" 'org-export-dispatch
;;     ",n" 'outline-next-visible-heading
;;     ",p" 'outline-previous-visible-heading
;;     ",t" 'org-set-tags-command
;;     ",u" 'outline-up-heading
;;     "$" 'org-end-of-line ; smarter behaviour on headlines etc.
;;     "^" 'org-beginning-of-line ; ditto
;;     "-" 'org-ctrl-c-minus ; change bullet style
;;     "<" 'org-metaleft ; out-dent
;;     ">" 'org-metaright ; indent
;;     ))
;; (prelude-evil-key-bindings-for-org)


(use-package org
  :defer t
  :bind (("C-c a" . org-agenda)
         ("C-c B" . org-switchb)
         :map org-mode-map
         ("C-c l" . org-store-link))
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (org-indent-mode 1)
                             (diminish 'org-indent-mode)))
  :config
  (setq org-agenda-files '("~/org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")))
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (add-to-list 'org-export-backends 'md)

  ;; FIXME org-agenda-execute-calendar-command uses deprecated list-calendar-holidays
  (unless (fboundp 'list-calendar-holidays)
    (defalias 'list-calendar-holidays 'calendar-list-holidays))

  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (python . t)
                                 (ruby . t)))

  (use-package org-bullets
    :defer t
    :ensure t
    :init
    (setq org-bullets-bullet-list
          '("✡" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))
    (add-hook 'org-mode-hook #'org-bullets-mode))

  ;; Presentation
  (use-package org-tree-slide
    :defer t
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
    :defer t
    :init (with-eval-after-load 'org-agenda
            (bind-key "P" 'org-pomodoro org-agenda-mode-map)))

  ;; Visually summarize progress
  (use-package org-dashboard
    :defer t)
  )

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
