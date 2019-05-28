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
  :diminish (dashboard-mode page-break-lines-mode)
  :defines (persp-save-dir persp-special-last-buffer)
  :functions (all-the-icons-faicon
              all-the-icons-material
              open-custom-file
              persp-get-buffer-or-null
              persp-load-state-from-file
              persp-switch-to-buffer
              winner-undo
              widget-forward)
  :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
  :init (dashboard-setup-startup-hook)
  :config
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
  (setq dashboard-banner-logo-title (format "Happy Hacking, %s - Emacs ♥ You!" kevin-user-name)
        dashboard-startup-banner (expand-file-name "vendor/banners/spacemacs.png" user-emacs-directory)
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5))
        dashboard-set-heading-icons t
        dashboard-heading-icons '((recents   . "file-text")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "file-directory")
                                  (registers . "database")))

  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defvar homepage-url "https://github.com/lkzz/emacs.d"
    "My emacs config github homepage.")

  (defun my-banner-path (&rest _)
    "Return the full ,@restpath to banner."
    (expand-file-name "banner.txt" user-emacs-directory))
  (advice-add #'dashboard-get-banner-path :override #'my-banner-path)

  (defun browse-homepage ()
    "Browse the github page of Emacs."
    (interactive)
    (browse-url homepage-url))

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (if (> (length (window-list-1))
           ;; exclude `treemacs' window
           (if (and (fboundp 'treemacs-current-visibility)
                    (eq (treemacs-current-visibility) 'visible))
               2
             1))
        (setq dashboard-recover-layout-p t))

    (delete-other-windows)

    ;; Refresh dashboard buffer
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)

    ;; Jump to the first section
    (goto-char (point-min))
    (dashboard-goto-recent-files))

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

  ;; Add file icons
  ;; MUST redefine the sections because of the macro `dashboard-insert-section-list'
  (defmacro dashboard-insert-section-list (section-name list action &rest rest)
    "Insert into SECTION-NAME a LIST of items, expanding ACTION and passing REST to widget creation."
    `(when (car ,list)
       (mapc (lambda (el)
               (let ((widget nil))
                 (insert "\n    ")
                 (when (display-graphic-p)
                   (insert (when-let ((path (car (last (split-string ,@rest " - ")))))
                             (if (file-directory-p path)
                                 (cond
                                  ((and (fboundp 'tramp-tramp-file-p)
                                        (tramp-tramp-file-p default-directory))
                                   (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                                  ((file-symlink-p path)
                                   (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                                  ((all-the-icons-dir-is-submodule path)
                                   (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                                  ((file-exists-p (format "%s/.git" path))
                                   (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                                  (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                                       (apply (car matcher) (list (cadr matcher) :v-adjust 0.01)))))
                               (all-the-icons-icon-for-file (file-name-nondirectory path)))))
                   (insert "\t"))
                 (setq widget
                       (widget-create 'push-button
                                      :action ,action
                                      :mouse-face 'highlight
                                      :button-prefix ""
                                      :button-suffix ""
                                      :format "%[%t%]"
                                      ,@rest))))
             ,list)))

  ;; Recentf
  (defun dashboard-insert-recents (list-size)
    "Add the list of LIST-SIZE items from recently edited files."
    (recentf-mode)
    (dashboard-insert-section
     "Recent Files:"
     recentf-list
     list-size
     "r"
     `(lambda (&rest ignore) (find-file-existing ,el))
     (abbreviate-file-name el)))

  ;; Bookmarks
  (defun dashboard-insert-bookmarks (list-size)
    "Add the list of LIST-SIZE items of bookmarks."
    (require 'bookmark)
    (dashboard-insert-section
     "Bookmarks:"
     (dashboard-subseq (bookmark-all-names)
                       0 list-size)
     list-size
     "m"
     `(lambda (&rest ignore) (bookmark-jump ,el))
     (let ((file (bookmark-get-filename el)))
       (if file
           (format "%s - %s" el (abbreviate-file-name file))
         el))))

  ;; Projectile
  (defun dashboard-insert-projects (list-size)
    "Add the list of LIST-SIZE items of projects."
    (require 'projectile)
    (projectile-load-known-projects)
    (dashboard-insert-section
     "Projects:"
     (dashboard-subseq (projectile-relevant-known-projects)
                       0 list-size)
     list-size
     "p"
     `(lambda (&rest ignore) (projectile-switch-project-by-name ,el))
     (abbreviate-file-name el)))

  (defun dashboard-center-line (&optional real-width)
    "When point is at the end of a line, center it.
REAL-WIDTH: the real width of the line.  If the line contains an image, the size
            of that image will be considered to be 1 by the calculation method
            used in this function.  As a consequence, the caller must calculate
            himself the correct length of the line taking into account the
            images he inserted in it."
    (let* ((width (or real-width (current-column)))
           (margin (max 0 (floor (/ (- dashboard-banner-length width) 2)))))
      (beginning-of-line)
      (insert (make-string margin ?\s))
      (end-of-line)))

  (defun dashboard-insert-navigator ()
    "Insert navigator below the banner."
    (interactive)
    (with-current-buffer (get-buffer dashboard-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (search-forward dashboard-banner-logo-title nil t)
        (insert "\n\n")
        (widget-create 'url-link
                       :tag (concat
                             (when (display-graphic-p)
                               (concat
                                (all-the-icons-octicon "mark-github"
                                                       :height 1.1
                                                       :v-adjust 0.0
                                                       :face 'font-lock-keyword-face)
                                (propertize " " 'face 'variable-pitch)))
                             (propertize "Homepage" 'face 'font-lock-keyword-face))
                       :help-echo "Browse homepage"
                       :mouse-face 'highlight
                       :button-prefix (propertize "[" 'face '(:inherit (font-lock-keyword-face bold)))
                       :button-suffix (propertize "]" 'face '(:inherit (font-lock-keyword-face bold)))
                       :format "%[%t%]"
                       homepage-url)
        (insert "  ")
        (widget-create 'push-button
                       :tag (concat
                             (when (display-graphic-p)
                               (concat
                                (all-the-icons-faicon "floppy-o"
                                                      :height 1.2
                                                      :v-adjust -0.1
                                                      :face 'font-lock-keyword-face)
                                (propertize " " 'face 'variable-pitch)))
                             (propertize "Open Config" 'face 'font-lock-keyword-face))
                       :action (lambda (&rest _) (dashboard-open-init-file))
                       :help-echo "Open init config"
                       :mouse-face 'highlight
                       :button-prefix (propertize "[" 'face '(:inherit (font-lock-keyword-face bold)))
                       :button-suffix (propertize "]" 'face '(:inherit (font-lock-keyword-face bold)))
                       :format "%[%t%]")
        (insert "  ")
        (widget-create 'push-button
                       :tag (concat
                             (when (display-graphic-p)
                               (concat
                                (all-the-icons-material "restore"
                                                        :height 1.35
                                                        :v-adjust -0.24
                                                        :face 'font-lock-keyword-face)
                                (propertize " " 'face 'variable-pitch)))
                             (propertize "Session" 'face 'font-lock-keyword-face))
                       :action (lambda (&rest _) (restore-session))
                       :help-echo "Restore previous session"
                       :mouse-face 'highlight
                       :button-prefix (propertize "[" 'face '(:inherit (font-lock-keyword-face bold)))
                       :button-suffix (propertize "]" 'face '(:inherit (font-lock-keyword-face bold)))
                       :format "%[%t%]")
        (dashboard-center-line))))
  (add-hook 'dashboard-mode-hook #'dashboard-insert-navigator)

  (defun dashboard-insert-footer ()
    "Insert footer of dashboard."
    (interactive)
    (with-current-buffer (get-buffer dashboard-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n")
        (insert (if (display-graphic-p)
                    (all-the-icons-faicon "heart"
                                          :height 1.1
                                          :v-adjust -0.05
                                          :face 'error)
                  "♥"))
        (insert " ")
        (insert (propertize
                 (format "Powered by %s, %s" kevin-user-name (format-time-string "%Y"))
                 'face font-lock-doc-face))
        (dashboard-center-line))))
  (add-hook 'dashboard-mode-hook #'dashboard-insert-footer)

  )

(provide 'init-dashboard)

;;; init-dashboard.el ends here
