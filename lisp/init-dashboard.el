;;; init-dashboard.el --- dashboard configuration -*- lexical-binding: t -*-
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
;;      refer: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-dashboard.el
;;; Code:

(eval-when-compile (require 'wid-edit))

(use-package dashboard
  :if (display-graphic-p)
  :diminish page-break-lines-mode
  :preface
  (defvar dashboard-recover-layout-p nil)
  (defvar homepage-url "https://github.com/lkzz/emacs.d")

  (defun browse-homepage ()
    "Browse the github page of Emacs."
    (interactive)
    (browse-url homepage-url))

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (dashboard-goto-recent-files)
    (if (> (length (window-list-1))
           ;; exclude `treemacs' window
           (if (and (fboundp 'treemacs-current-visibility)
                    (eq (treemacs-current-visibility) 'visible))
               2
             1))
        (setq dashboard-recover-layout-p t))
    (delete-other-windows))

  (defun restore-session ()
    "Restore last session."
    (interactive)
    (when (bound-and-true-p persp-mode)
      (message "Restoring session...")
      (condition-case-unless-debug err
          (persp-load-state-from-file)
        (error
         (message "Error: Unable to restore last session -- %s" err)))
      (when (persp-get-buffer-or-null persp-special-last-buffer)
        (persp-switch-to-buffer persp-special-last-buffer))))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when (and dashboard-recover-layout-p
               (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil)))

  (defun dashboard-open-init-file ()
    "Open init config file."
    (interactive)
    (quit-dashboard)
    (kevin/open-init-file))

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (funcall (local-key-binding "r")))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (funcall (local-key-binding "p")))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (funcall (local-key-binding "m")))
  :init (setq initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  :hook (after-init . dashboard-setup-startup-hook)
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("H" . browse-homepage)
         ("O" . dashboard-open-init-file)
         ("R" . restore-session)
         ("q" . quit-dashboard))
  :general
  (general-nmap dashboard-mode-map
    "TAB" 'widget-forward
    "RET" 'widget-button-press
    "g" 'dashboard-refresh-buffer
    "}" 'dashboard-next-section
    "{" 'dashboard-previous-section
    "p" 'dashboard-goto-projects
    "m" 'dashboard-goto-bookmarks
    "r" 'dashboard-goto-recent-files
    "H" 'browse-homepage
    "R" 'restore-session
    "O" 'dashboard-open-init-file
    "q" 'quit-dashboard)
  :config
  (setq dashboard-banner-logo-title (format "Happy Hacking, %s - Emacs ♥ You!" kevin-user-name))
  (setq dashboard-startup-banner (expand-file-name "vendor/banners/spacemacs.png" user-emacs-directory))
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 3)))

  (defun dashboard-insert-buttons (_list-size)
    (insert "\n")
    (insert (make-string (max 0 (floor (/ (- dashboard-banner-length 51) 2))) ?\ ))
    (widget-create 'url-link
                   :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                   :help-echo "Open Emacs Github page"
                   :mouse-face 'highlight
                   homepage-url)
    (insert " ")
    (widget-create 'push-button
                   :help-echo "Open Personal Configurations"
                   :action (lambda (&rest _) (dashboard-open-init-file))
                   :mouse-face 'highlight
                   :button-prefix ""
                   :button-suffix ""
                   (propertize "Open Config" 'face 'font-lock-keyword-face))
    (insert " ")
    (widget-create 'push-button
                   :help-echo "Restore previous session"
                   :action (lambda (&rest _) (restore-session))
                   :mouse-face 'highlight
                   :button-prefix ""
                   :button-suffix ""
                   (propertize "Restore Session" 'face 'font-lock-keyword-face))
    (insert "\n")
    (insert "\n")
    ;; (insert "\n")
    (insert (format "[%d packages loaded in %s]" (length package-activated-list) (emacs-init-time))))
  (add-to-list 'dashboard-item-generators  '(buttons . dashboard-insert-buttons))
  (add-to-list 'dashboard-items '(buttons))
  (dashboard-insert-startupify-lists))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
