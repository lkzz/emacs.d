;;; ui.el --- insert description here -*- lexical-binding: t -*-
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
(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.
 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
  (list
   (cond
	((or (string-equal "*" (substring (buffer-name) 0 1))
	     (memq major-mode '(magit-process-mode
				            magit-status-mode
				            magit-diff-mode
				            magit-log-mode
				            magit-file-mode
				            magit-blob-mode
				            magit-blame-mode
                            emacs-lisp-mode)))
	 "Emacs")
	((derived-mode-p 'prog-mode)
	 "Coding")
	((derived-mode-p 'dired-mode)
	 "Dired")
	((memq major-mode '(helpful-mode
			            help-mode))
	 "Help")
	((memq major-mode '(org-mode
			            org-agenda-clockreport-mode
			            org-src-mode
			            org-agenda-mode
			            org-beamer-mode
			            org-indent-mode
			            org-bullets-mode
			            org-cdlatex-mode
			            org-agenda-log-mode
			            diary-mode))
	 "Org")
	(t
	 (centaur-tabs-get-group-name (current-buffer))))))

;;;###autoload
(defun kevin/enable-menu-bar-in-gui (&optional frame)
  "Re-enable menu-bar-lines in GUI frames."
  (when-let (frame (or frame (selected-frame)))
    (when (display-graphic-p frame)
      (set-frame-parameter frame 'menu-bar-lines 1))))

;;;###autoload
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))
