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

(use-package dashboard
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner (expand-file-name "vendor/banners/spacemacs.png" user-emacs-directory)
        initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-banner-logo-title (format "Happy Hacking, %s - Emacs ♥ You!" kevin-user-name)
        dashboard-center-content t
        dashboard-set-init-info t
        dashboard-init-info (format "%d packages loaded in %s"
                                    (length package-activated-list) (emacs-init-time))
        dashboard-show-shortcuts nil
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5))
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-heading-icons '((recents   . "file-text")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "file-directory")
                                  (registers . "database"))
        dashboard-set-footer t
        dashboard-footer (format "Powered by %s, %s" kevin-user-name (format-time-string "%Y"))
        dashboard-footer-icon (if (display-graphic-p)
                                  (all-the-icons-faicon "heart"
                                                        :height 1.1
                                                        :v-adjust -0.05
                                                        :face 'error)
                                "♥")
        dashboard-set-navigator t

        ;; Format: "icon title help action face prefix suffix"
        dashboard-navigator-buttons
        `(
          (,(and (display-graphic-p)
                 (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
           "Homepage"
           "Browse homepage"
           (lambda (&rest _) (kevin/browse-homepage)))

          (,(and (display-graphic-p)
                 (all-the-icons-faicon "floppy-o" :height 1.2 :v-adjust -0.1 :face 'font-lock-keyword-face))
           "Open Config"
           "Open init config"
           (lambda (&rest _) (kevin/open-init-file)))

          (,(and (display-graphic-p)
                 (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24 :face 'font-lock-keyword-face))
           "Restore"
           "Restore session"
           (lambda (&rest _) (kevin/restore-session)))
          )
        )

  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defun kevin/browse-homepage ()
    "Browse the github page of Emacs."
    (interactive)
    (browse-url "https://github.com/lkzz/emacs.d"))

  (defun kevin/quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when (and dashboard-recover-layout-p
               (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil)))

  (defun kevin/dashboard-open-init-file ()
    "Open init config file."
    (interactive)
    (kevin/quit-dashboard)
    (kevin/open-init-file))

  (defun kevin/restore-session ()
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

  (defun kevin/dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (funcall (local-key-binding "r")))

  (defun kevin/dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (funcall (local-key-binding "p")))

  (defun kevin/dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (funcall (local-key-binding "m")))

  (general-nmap dashboard-mode-map
    "TAB" 'widget-forward
    "RET" 'widget-button-press
    "g" 'dashboard-refresh-buffer
    "}" 'dashboard-next-section
    "{" 'dashboard-previous-section
    "p" 'kevin/dashboard-goto-projects
    "m" 'kevin/dashboard-goto-bookmarks
    "r" 'kevin/dashboard-goto-recent-files
    "H" 'kevin/browse-homepage
    "R" 'kevin/restore-session
    "O" 'kevin/dashboard-open-init-file
    "q" 'kevin/quit-dashboard)
  )

(provide 'init-dashboard)

;;; init-dashboard.el ends here
