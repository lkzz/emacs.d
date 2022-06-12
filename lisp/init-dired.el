;;; init-dired.el --- config dired mode. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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
  :defer t
  :straight (:type built-in)
  :init
  (setq dired-dwim-target t            ; select another buffer as target when this is two dired buffer
        dired-isearch-filenames 'dwim  ; Search file name only when focus is over file
        dired-recursive-copies 'always ; always copy recursively
        dired-recursive-deletes 'top   ; always delete recursively
        dired-auto-revert-buffer t
        dired-hide-details-hide-symlink-targets nil)
  :config
  (general-evil-define-key 'normal dired-mode-map
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
    "th" 'dired-omit-mode
    "tt" 'dired-toggle-marks
    "u" 'dired-unmark
    "v" 'dired-git-info-mode
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
      (kill-buffer orig)))

  ;; Colourful dired
  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

  ;; Shows icons
  (use-package all-the-icons-dired
    :if (display-graphic-p)
    :straight (:host github :repo "wyuenho/all-the-icons-dired") ;; use fork version
    :diminish
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    (setq all-the-icons-dired-monochrome nil))

  (use-package dired-x
    :straight (:type built-in)
    :diminish dired-omit-mode
    :hook (dired-mode . dired-omit-mode)
    :config
    (setq dired-omit-verbose nil)
    (let ((cmd (cond
                (is-mac-p "open")
                (is-linux-p "xdg-open")
                (my/window-p "start")
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
    (setq dired-omit-files (concat dired-omit-files
                                   "\\|^\\..*"
                                   "\\|^bazel*"))))

(provide 'init-dired)
;;; init-dired ends here
