;;; init-dired.el --- config dired mode. -*- lexical-binding: t; -*-
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
;;
;;; Code:

(use-package dired
  :ensure nil
  :init
  (kevin/space-key-define "jd" 'dired-jump)
  (setq dired-recursive-copies 'always ; always copy recursively
        dired-recursive-deletes 'top   ; always delete recursively
        dired-auto-revert-buffer t
        dired-hide-details-hide-symlink-targets nil)
  :general
  (general-nmap dired-mode-map
    ;; Lower keys for commands not operating on all the marked files
    "a" 'dired-find-alternate-file
    "d" 'dired-flag-file-deletion
    "gf" 'dired-find-file
    "gy" 'dired-show-file-type
    "gr" 'revert-buffer
    "h" 'dired-up-directory
    "i" 'dired-toggle-read-only
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "l" 'dired-find-file
    "m" 'dired-mark
    "o" 'dired-sort-toggle-or-edit
    "q" 'quit-window
    "r" 'dired-do-redisplay
    "tt" 'dired-toggle-marks
    "u" 'dired-unmark
    "x" 'dired-do-flagged-delete
    "RET" 'dired-find-file
    ;; Commands to mark or flag certain categories of files
    "+" 'dired-create-directory
    "^" 'dired-up-directory
    "#" 'dired-flag-auto-save-files
    "." 'dired-clean-directory
    "~" 'dired-flag-backup-files
    "!" 'dired-do-shell-command
    "&" 'dired-do-async-shell-command
    ;; Upper case keys (except !) for operating on the marked files
    "A" 'dired-do-find-regexp
    "C" 'dired-do-copy
    "B" 'dired-do-byte-compile
    "D" 'dired-do-delete
    "G" 'dired-do-chgrp
    "H" 'dired-do-hardlink
    "I" 'dired-maybe-insert-subdir
    "J" 'dired-goto-file
    "K" 'dired-do-kill-lines
    "L" 'dired-do-load
    "M" 'dired-do-chmod
    "O" 'dired-do-chown
    "P" 'dired-do-print
    "Q" 'dired-do-find-regexp-and-replace
    "R" 'dired-do-rename
    "S" 'dired-do-symlink
    "T" 'dired-do-touch
    "W" 'browse-url-of-dired-file
    "X" 'dired-do-shell-command
    "Y" 'dired-copy-filename-as-kill
    "Z" 'dired-do-compress)
  :config
  ;; Search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  ;; when there is two dired buffer, Emacs will select another buffer
  ;; as target buffer (target for copying files, for example).
  ;; It's similar to windows commander.
  (setq dired-dwim-target t)
  (when is-mac-p
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls") ; brew install coreutils
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))
  (when (or (and is-mac-p (executable-find "gls"))
            (and (not is-mac-p) (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)
    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first"))
  ;; Use single buffer
  (defadvice dired-find-file (around dired-find-file-single-buffer activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-file-for-visit)))
      ad-do-it
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))
  (defadvice dired-up-directory (around dired-up-directory-single-buffer activate)
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer)))
      ad-do-it
      (kill-buffer orig))))

;; Colourful dired
(use-package diredfl
  :defer t
  :hook (dired-mode . diredfl-mode))

;; Shows icons
(use-package all-the-icons-dired
  :defer t
  :after all-the-icons
  :diminish
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground nil))))
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-x
  :ensure nil
  :defer t
  :diminish dired-omit-mode
  :hook (dired-mode . dired-omit-mode)
  :general
  (general-nmap dired-mode-map
    "th" 'dired-omit-mode)
  :config
  (let ((cmd (cond
              (is-mac-p "open")
              (is-linux-p "xdg-open")
              (kevin-window-p "start")
              (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
  ;; Don’t ask whether to kill buffers visiting deleted files
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^\\..*\\|^bazel*")))

;; Show git info in dired
(use-package dired-git-info
  :after dired
  :general
  (general-nmap dired-mode-map
    "v" 'dired-git-info-mode))

(provide 'init-dired)
;;; init-dired ends here
