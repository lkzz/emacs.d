;;; dired.el --- dired mode -*- lexical-binding: t -*-
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
(defun my-all-the-icons-dired--refresh ()
  "Display the icons of files in a dired buffer."
  (all-the-icons-dired--remove-all-overlays)
  ;; NOTE: don't display icons it too many items
  (if (<= (count-lines (point-min) (point-max)) 1000)
      (save-excursion
        ;; TRICK: Use TAB to align icons
        (setq-local tab-width 1)

        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((file (dired-get-filename 'verbatim t)))
            (let ((icon (if (file-directory-p file)
                            (all-the-icons-icon-for-dir
                             file
                             :face 'all-the-icons-dired-dir-face
                             :height 1.0
                             :v-adjust all-the-icons-dired-v-adjust)
                          (all-the-icons-icon-for-file
                           file
                           :height 1.0
                           :v-adjust all-the-icons-dired-v-adjust))))
              (if (member file '("." ".."))
                  (all-the-icons-dired--add-overlay (point) "  \t")
                (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))
          (dired-next-line 1)))
    (message "Not display icons because of too many items.")))
