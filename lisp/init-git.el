;;; init-git.el --- version contral setup. -*- lexical-binding: t; -*-
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
  :general
  (kevin/space-key-define
    "g" '(:ignore t :which-key "Git")
    "g a" '(kevin/git-add-current-file :wk "add-current-file")
    "g b" 'magit-blame
    "g c" '(kevin/git-checkout-current-file :wk "checkout-current-file")
    "g d" 'magit-diff-buffer-file
    "g i" 'magit-init
    "g l" 'magit-log-buffer-file
    "g L" 'magit-list-repositories
    "g s" 'magit-status
    "g S" 'magit-stage-file
    "g u" 'magit-unstage-file
    "g v" 'vc-annotate)
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
  :bind (("C-x v m" . git-messenger:popup-message))
  :commands (git-messenger:copy-message
             git-messenger:popup-message
             git-messenger:show-detail)
  :general
  (kevin/space-key-define "g m" '(git-messenger:popup-message :wk "popup-message"))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t
        git-messenger:show-detail t))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :init
  (defhydra hydra-git-timemachine (:body-pre (unless (bound-and-true-p git-timemachine-mode)
                                               (call-interactively 'git-timemachine))
                                             :post (git-timemachine-quit)
                                             :color pink ;; toggle :foreign-keys run
                                             :hint nil)
    "
[_p_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit
"
    ("c" git-timemachine-show-current-revision)
    ("g" git-timemachine-show-nth-revision)
    ("p" git-timemachine-show-previous-revision)
    ("n" git-timemachine-show-next-revision)
    ("Y" git-timemachine-kill-revision)
    ("q" nil exit: t))
  (kevin/space-key-define "g t" '(hydra-git-timemachine/body :wk "git-timemachine")))

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
  :init
  (defhydra hydra-smerge-mode (:color pink :hint nil)
    "
^Keep^            ^Diff^            ^Move
^^----------------------------------------------
_b_: base         _<_: upper        _n_: next
_u_: upper        _>_: lower        _p_: previous
_l_: lower        _=_: all          _s_: swap
_a_: all          _e_: ediff        _q_: quit
"
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("<" smerge-diff-base-upper)
    (">" smerge-diff-base-lower)
    ("=" smerge-diff-upper-lower)
    ("e" smerge-ediff)
    ("n" smerge-next)
    ("p" smerge-prev)
    ("s" smerge-swap)
    ("q" nil :exit t))
  (kevin/space-key-define "g r" '(hydra-smerge-mode/body :wk "hydra-smerge-mode")))

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
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh))
  :init
  (defhydra hydra-diff-hl (:color pink :hint nil)
    "
_p_: previous _n_: next _m_: mark _g_: goto nth _r_: revert _q_: quit"
    ("p" diff-hl-previous-hunk)
    ("n" diff-hl-next-hunk)
    ("m" diff-hl-mark-hunk)
    ("g" diff-hl-diff-goto-hunk)
    ("r" diff-hl-revert-hunk)
    ("q" nil exit: t))
  (kevin/space-key-define "g h" '(hydra-diff-hl/body :wk "hydra-diff-hl"))
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
    (diff-hl-margin-mode 1)))

(provide 'init-git)
;;; init-git ends here
