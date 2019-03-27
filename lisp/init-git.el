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
  :commands (magit-file-log magit-blame-mode)
  :bind
  (("C-x g i" . magit-init)
   ("C-x g f" . magit-file-log)
   ("C-x g b" . magit-blame-mode)
   ("C-x g m" . magit-branch-manager)
   ("C-x g c" . magit-branch)
   ("C-x g s" . magit-status)
   ("C-x g r" . magit-reflog)
   ("C-x g t" . magit-tag))
  :init
  (kevin/declare-prefix "g" "magit")
  (kevin/set-leader-keys "ga" 'kevin/git-add-current-file
                         "gc" 'kevin/git-checkout-current-file
                         "gd" 'magit-diff-buffer-file
                         "gl" 'magit-log-buffer-file
                         "gi" 'magit-init
                         "gL" 'magit-list-repositories
                         "gs" 'magit-status
                         "gS" 'magit-stage-file
                         "gU" 'magit-unstage-file
                         "gv" 'vc-annotate)
  :config
  ;; display buffer fullframe
  (setq magit-display-buffer-function #'kevin/magit-display-buffer-function)
  ;; `git-commit-mode'
  ;; see https://chris.beams.io/posts/git-commit/
  (setq fill-column 72
        git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

(use-package evil-magit
  :after (evil magit)
  :init
  (setq evil-magit-want-horizontal-movement nil)
  (add-hook 'git-commit-mode-hook #'evil-insert-state))

;; Gitflow externsion for Magit
(use-package magit-gitflow
  :after magit
  :diminish magit-gitflow-mode
  :bind (:map magit-status-mode-map
              ("G" . magit-gitflow-popup))
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow)
  :config
  (magit-define-popup-action 'magit-dispatch-popup
                             ?G "GitFlow" #'magit-gitflow-popup ?!))

;;; Pop up last commit information of current line
(use-package git-messenger
  :commands (git-messenger:copy-message git-messenger:popup-message)
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t)
  (setq git-messenger:show-detail t)
  (kevin/set-leader-keys "gm"  'git-messenger:popup-message))

;; Walk through git revisions of a file
(use-package git-timemachine
  :commands (hydra-git-timemachine/body)
  :init
  (kevin/set-leader-keys "gt" #'hydra-git-timemachine/body)
  ;; (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state)
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
    ("q" nil exit: t)))

;; Git modes
(use-package gitconfig-mode
  :mode (("/\\.?git/?config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)
         ("/_gitconfig\\'" . gitconfig-mode))
  :config
  (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)))

(use-package git-link
  :config
  (kevin/set-leader-keys "gl" 'git-link-commit)
  (setq git-link-open-in-browser t))

(use-package smerge
  :ensure nil
  :commands (smerge-mode)
  :init
  (defhydra hydra-smerge-mode (:hint nil
                                     :pre (smerge-mode 1)
                                     ;; Disable `smerge-mode' when quitting hydra if
                                     ;; no merge conflicts remain.
                                     :post (smerge-auto-leave))
    "
                                                    ╭────────┐
  Movement   Keep           Diff              Other │ smerge │
  ╭─────────────────────────────────────────────────┴────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff             ╭──────────
     ^_G_^                                            │ [_q_] quit"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("C-j" smerge-next)
    ("C-k" smerge-prev)
    ("j" next-line)
    ("k" previous-line)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ("q" nil :color blue))
  (kevin/set-leader-keys "gr" #'hydra-smerge-mode/body)
  (defun kevin/enable-smerge-mode-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode 1)
        (when (featurep 'hydra)
          (hydra-smerge-mode/body))
        )))
  (add-hook 'find-file-hook #'kevin/enable-smerge-mode-maybe)
  )

;; Highlight uncommitted changes
(use-package diff-hl
  :commands (diff-hl-next-hunk diff-hl-previous-hunk)
  :custom-face
  (diff-hl-insert ((t (:background "#7ccd7c"))))
  (diff-hl-change ((t (:background "#3a81c3"))))
  (diff-hl-delete ((t (:background "#ee6363"))))
  (diff-hl-margin-insert ((t (:background "#7ccd7c"))))
  (diff-hl-margin-change ((t (:background "#3a81c3"))))
  (diff-hl-margin-delete ((t (:background "#ee6363"))))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init
  (defhydra hydra-diff-hl (:color pink
                                  :hint nil)
    "
[_p_] previous hunk [_n_] next hunk [_r_] revert hunk [_q_] quit\n
"
    ("p" diff-hl-previous-hunk)
    ("n" diff-hl-next-hunk)
    ("r" diff-hl-revert-hunk)
    ("q" nil exit: t))
  (kevin/set-leader-keys "gh" #'hydra-diff-hl/body)
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
