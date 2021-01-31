;;; dashboard.el --- dashboard autoload functions. -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;; Code:


;;;###autoload
(defun kevin/browse-homepage ()
  "Browse the github page of Emacs."
  (interactive)
  (browse-url "https://github.com/lkzz/emacs.d"))

(defvar dashboard-recover-layout-p nil
  "Wether recovers the layout.")

;;;###autoload
(defun kevin/quit-dashboard ()
  "Quit dashboard window."
  (interactive)
  (quit-window t)
  (when (and dashboard-recover-layout-p
             (bound-and-true-p winner-mode))
    (winner-undo)
    (setq dashboard-recover-layout-p nil)))

;;;###autoload
(defun kevin/dashboard-open-init-file ()
  "Open init config file."
  (interactive)
  (kevin/quit-dashboard)
  (kevin/open-init-file))

;;;###autoload
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

;;;###autoload
(defun kevin/dashboard-goto-recent-files ()
  "Go to recent files."
  (interactive)
  (funcall (local-key-binding "r")))

;;;###autoload
(defun kevin/dashboard-goto-projects ()
  "Go to projects."
  (interactive)
  (funcall (local-key-binding "p")))

;;;###autoload
(defun kevin/dashboard-goto-bookmarks ()
  "Go to bookmarks."
  (interactive)
  (funcall (local-key-binding "m")))
