;;; init-ranger.el -- setup ranger. -*- lexical-binding: t; -*-
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

(use-package ranger
  :init
  (kevin/set-leader-keys "jr" 'deer)
  :hook (ranger-mode . all-the-icons-dired-mode)
  :general
  (general-nmap ranger-mode-map
    "q" 'ranger-close)
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
        ranger-hide-cursor nil))

(provide 'init-ranger)
;;; init-ranger.el ends here
