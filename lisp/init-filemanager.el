;;; init-filemanager.el -- setup file manager by ranger and dired. -*- lexical-binding: t; -*-
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

(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :after all-the-icons)

(use-package dired
  :ensure nil
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always))

(use-package ranger
  :init
  (kevin/set-leader-keys "jd" 'deer)
  :hook (ranger-mode . all-the-icons-dired-mode)
  :config
  (setq ranger-override-dired t
        ranger-cleanup-on-disable t
        ranger-cleanup-eagerly t
        ranger-modify-header nil
        ;; To exclude certain files (e.g. videos) from being previewed
        ranger-excluded-extensions '("mkv" "iso" "mp4" "pdf")
        ;; Binary files will not be previewed, if this variable is set to t
        ranger-dont-show-binary t
        ranger-preview-file t
        ;; Files that are larger than the max file size (in MB) variable, won't be previewed.
        ranger-max-preview-size 20
        ;; Hide dotfiles
        ranger-show-hidden nil
        ranger-parent-depth 0
        ranger-max-parent-width 0.12
        ;; Show cursor in ranger
        ranger-hide-cursor nil)
  (with-eval-after-load 'evil
    (evil-define-key 'normal ranger-mode-map (kbd "q") 'ranger-close)))

(provide 'init-filemanager)
;;; init-filemanager.el ends here
