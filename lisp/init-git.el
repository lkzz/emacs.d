;;; init-git.el --- version contral setup. -*- lexical-binding: t; -*-
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

;;;###autload
(defun git-get-current-file-relative-path ()
  "Get current file relative path."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

;;;###autload
(defun kevin/git-checkout-current-file ()
  "Git checkout current file."
  (interactive)
  (when (and (buffer-file-name)
             (yes-or-no-p (format "git checkout %s?"
                                  (file-name-nondirectory (buffer-file-name)))))
    (let* ((filename (git-get-current-file-relative-path)))
      (shell-command (concat "git checkout " filename))
      (kevin/revert-buffer-no-confirm)
      (message "DONE! git checkout %s" filename))))

;;;###autload
(defun kevin/git-add-current-file ()
  "Git add file of current buffer."
  (interactive)
  (let ((filename))
    (when buffer-file-name
      (setq filename (git-get-current-file-relative-path))
      (shell-command (concat "git add " filename))
      (message "DONE! git add %s" filename))))

;;;###autload
(defun kevin/magit-display-buffer-function (buffer)
  (if magit-display-buffer-noselect
      ;; the code that called `magit-display-buffer-function'
      ;; expects the original window to stay alive, we can't go
      ;; fullscreen
      (magit-display-buffer-traditional buffer)
    (delete-other-windows)
    ;; make sure the window isn't dedicated, otherwise
    ;; `set-window-buffer' throws an error
    (set-window-dedicated-p nil nil)
    (set-window-buffer nil buffer)
    ;; return buffer's window
    (get-buffer-window buffer)))

(use-package magit
  :defer t
  :init
  (kevin/declare-prefix "g" "magit")
  (kevin/set-leader-keys
    "ga" #'kevin/git-add-current-file
    "gb" 'magit-blame
    "gc" #'kevin/git-checkout-current-file
    "gd" 'magit-diff-buffer-file
    "gl" 'magit-log-buffer-file
    "gi" 'magit-init
    "gL" 'magit-list-repositories
    "gs" 'magit-status
    "gS" 'magit-stage-file
    "gu" 'magit-unstage-file
    "gv" 'vc-annotate)
  :config
  ;; display buffer fullframe
  (setq magit-display-buffer-function #'kevin/magit-display-buffer-function)
  ;; see https://chris.beams.io/posts/git-commit/
  (setq fill-column 72
        git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

(use-package evil-magit
  :after (evil magit)
  :init (evil-magit-init)
  :hook (git-commit-mode . evil-insert-state))

;;; Pop up last commit information of current line
(use-package git-messenger
  :commands (git-messenger:copy-message
             git-messenger:popup-message)
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t)
  (setq git-messenger:show-detail t)
  (kevin/set-leader-keys "gm" 'git-messenger:popup-message))

;; Walk through git revisions of a file
(use-package git-timemachine
  :commands (hydra-git-timemachine/body)
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit font-lock-string-face))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :preface
  (defhydra hydra-git-timemachine (:body-pre (unless (bound-and-true-p git-timemachine-mode)
                                               (call-interactively 'git-timemachine))
                                             :post (git-timemachine-quit)
                                             :color pink ;; toggle :foreign-keys run
                                             :hint nil)
    "
[_p_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit\n
"
    ("c" git-timemachine-show-current-revision)
    ("g" git-timemachine-show-nth-revision)
    ("p" git-timemachine-show-previous-revision)
    ("n" git-timemachine-show-next-revision)
    ("Y" git-timemachine-kill-revision)
    ("q" nil exit: t))
  :init
  (kevin/set-leader-keys "gt" #'hydra-git-timemachine/body))

;; Git modes
(use-package gitconfig-mode
  :mode (("/\\.?git/?config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)
         ("/_gitconfig\\'" . gitconfig-mode)))

(use-package gitignore-mode
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)))

(use-package smerge-mode
  :ensure nil
  :diminish smerge-mode
  :pretty-hydra
  ((:color pink :quit-key "q")
   ("Move"
    (("n" smerge-next "next")
     ("p" smerge-prev "previous"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "upper")
     ("l" smerge-keep-lower "lower")
     ("a" smerge-keep-all "all")
     ("RET" smerge-keep-current "current")
     ("C-m" smerge-keep-current "current"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
     ("=" smerge-diff-upper-lower "upper/lower")
     (">" smerge-diff-base-lower "upper/lower")
     ("R" smerge-refine "refine")
     ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill"))))
  :init (kevin/set-leader-keys "gr" #'smerge-mode-hydra/body)
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)
                            (smerge-mode-hydra/body)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-mode-hydra/body))))))

;; Highlight uncommitted changes
(use-package diff-hl
  :commands (diff-hl-next-hunk
             diff-hl-previous-hunk)
  :custom-face
  (diff-hl-insert ((t (:background "#7ccd7c"))))
  (diff-hl-change ((t (:background "#3a81c3"))))
  (diff-hl-delete ((t (:background "#ee6363"))))
  (diff-hl-margin-insert ((t (:background "#7ccd7c"))))
  (diff-hl-margin-change ((t (:background "#3a81c3"))))
  (diff-hl-margin-delete ((t (:background "#ee6363"))))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :pretty-hydra
  ((:color pink :quit-key "q")
   ("Move"
    (("p" diff-hl-previous-hunk)
     ("n" diff-hl-next-hunk))
    "Action"
    (("m" diff-hl-mark-hunk)
     ("=" diff-hl-diff-goto-hunk)
     ("r" diff-hl-revert-hunk))))
  :init
  (kevin/set-leader-keys "gh" #'diff-hl-hydra/body)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  ;; Set fringe style
  (setq-default fringes-outside-margins nil)
  (setq diff-hl-draw-borders nil)
  ;; Set diff-hl-margin-mode
  (unless (display-graphic-p)
    (setq diff-hl-margin-symbols-alist '((insert . " ")
                                         (delete . " ")
                                         (change . " ")
                                         (unknown . " ")
                                         (ignored . " ")))
    ;; Display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1))
  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))


(provide 'init-git)
;;; init-git ends here
