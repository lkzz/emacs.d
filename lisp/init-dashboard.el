;;; init-dashboard.el --- dashboard configuration -*- lexical-binding: t -*-
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
;;      refer: https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-dashboard.el
;;; Code:

(use-package dashboard
  :diminish page-break-lines-mode
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
  :init
  (setq dashboard-startup-banner (expand-file-name "banner/spacemacs.png" user-emacs-directory)
        dashboard-banner-logo-title (format "Happy Hacking, %s - Emacs ♥ You!" user-full-name)
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
        dashboard-footer-messages (list (format "Powered by %s, %s" user-full-name (format-time-string "%Y")))
        dashboard-footer-icon (if (display-graphic-p)
                                  (all-the-icons-faicon "heart"
                                                        :height 1.1
                                                        :v-adjust -0.05
                                                        :face 'error)
                                "♥")
        dashboard-set-navigator t
        ;; Format: "icon title help action face prefix suffix"
        dashboard-navigator-buttons
        `(((,(when (display-graphic-p)
               (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (kevin/browse-homepage)))

           (,(when (display-graphic-p)
               (all-the-icons-faicon "floppy-o" :height 1.2 :v-adjust -0.1 :face 'font-lock-keyword-face))
            "Open Config"
            "Open init config"
            (lambda (&rest _) (kevin/open-init-file)))

           (,(when (display-graphic-p)
               (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24 :face 'font-lock-keyword-face))
            "Restore"
            "Restore session"
            (lambda (&rest _) (kevin/restore-session))))))
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
