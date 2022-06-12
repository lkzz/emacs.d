;;; init-vc.el --- version control setup. -*- lexical-binding: t; -*-
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

(use-package magit
  :defer 3
  :init
  ;; Suppress the message we get about "Turning on magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode...")
        magit-diff-refine-hunk t)
  :config
  ;; Close transient with esc and q
  (define-key transient-map [escape] #'transient-quit-one)
  (define-key transient-map "q" #'transient-quit-one)
  ;; see https://chris.beams.io/posts/git-commit/
  (setq fill-column 72
        magit-auto-revert-mode t
        git-commit-summary-max-length 50
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)
        magit-display-buffer-function #'my/magit-display-buffer-function ; display buffer fullframe
        magit-bury-buffer-function #'my/magit-bury-buffer-function))     ; bury or kill the current magit buffer

;; Show TODOs in magit
(use-package magit-todos
  :after magit
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

;; Show git blame info
(use-package blamer
  :disabled
  :custom-face (blamer-face ((t (:inherit completions-annotations :height 0.9))))
  :init
  (setq blamer-idle-time 0.5
        blamer-min-offset 50
        blamer-type 'visual
        blamer-view 'overlay
        blamer-max-commit-message-length 50
        blamer-author-formatter "%s "
        blamer-datetime-formatter "[%s] "
        blamer-commit-formatter "- %s"))

(use-package smerge-mode
  :defer t
  :straight (:type built-in)
  :diminish smerge-mode
  :init
  (defhydra hydra-smerge-mode (:hint nil
                                     :pre (if (not smerge-mode) (smerge-mode 1))
                                     ;; Disable `smerge-mode' when quitting hydra if
                                     ;; no merge conflicts remain.
                                     :post (smerge-auto-leave))
    "
                                                         [smerge]
  Movement   Keep           Diff              Other
  ╭─────────────────────────────────────────────────────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff                 ╭──────────
     ^_G_^                                                │ [_q_] quit
"
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
  :hook (find-file . (lambda ()
                       (save-excursion
                         (goto-char (point-min))
                         (when (re-search-forward "^<<<<<<< " nil t)
                           (hydra-smerge-mode/body))))))


;; Git modes
(use-package git-modes)

;; Highlight uncommitted changes
(use-package diff-hl
  :commands (diff-hl-next-hunk diff-hl-previous-hunk)
  :custom-face
  (diff-hl-insert ((t (:inherit diff-added :background nil))))
  (diff-hl-delete ((t (:inherit diff-removed :background nil))))
  (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  (diff-hl-margin-insert ((t (:inherit diff-added :background nil))))
  (diff-hl-margin-delete ((t (:inherit diff-removed :background nil))))
  (diff-hl-margin-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (setq diff-hl-draw-borders nil)
  (defhydra hydra-diff-hl (:color pink :hint nil)
    "
_k_: previous _j_: next _m_: mark _g_: goto nth _r_: revert _q_: quit"
    ("j" diff-hl-next-hunk)
    ("k" diff-hl-previous-hunk)
    ("m" diff-hl-mark-hunk)
    ("g" diff-hl-diff-goto-hunk)
    ("r" diff-hl-revert-hunk)
    ("q" nil exit: t))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)
  ;; Set fringe style
  (setq-default fringes-outside-margins nil)

  ;; Reset faces after changing the color theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (custom-set-faces
               '(diff-hl-insert ((t (:inherit diff-added :background nil))))
               '(diff-hl-delete ((t (:inherit diff-removed :background nil))))
               `(diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil)))))))

  (defun my/diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my/diff-hl-bmp
      (vector (if is-mac-p #b11100000 #b11111100))
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function #'my/diff-hl-fringe-bmp-function)

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  ;; Set diff-hl-margin-mode
  (unless (display-graphic-p)
    (setq diff-hl-margin-symbols-alist '((insert . " ")
                                         (delete . " ")
                                         (change . " ")
                                         (unknown . " ")
                                         (ignored . " ")))
    ;; Display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)))

;; Walk through git revisions of a file
(use-package git-timemachine
  :defer t
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :hook ((git-timemachine-mode . (lambda ()
                                   "Display different colors in mode-line."
                                   (face-remap-add-relative 'mode-line 'custom-saved)))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer")))))
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
    ("q" nil exit: t)))

;; Pop up last commit information of current line
(use-package git-messenger
  :init
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)
  (defhydra git-messenger-hydra (:color blue)
    ("s" git-messenger:popup-show "show")
    ("c" git-messenger:copy-commit-id "copy hash")
    ("m" git-messenger:copy-message "copy message")
    ("q" git-messenger:popup-close "quit"))
  :config
  (defun my/git-messenger:format-detail (vcs commit-id author message)
    (if (eq vcs 'git)
        (let ((date (git-messenger:commit-date commit-id))
              (colon (propertize ":" 'face 'font-lock-comment-face)))
          (concat
           (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                   (propertize "Commit" 'face 'font-lock-keyword-face) colon
                   (propertize (substring commit-id 0 8) 'face 'font-lock-string-face)
                   (propertize "Author" 'face 'font-lock-keyword-face) colon
                   (propertize author 'face 'font-lock-string-face)
                   (propertize "Date" 'face 'font-lock-keyword-face) colon
                   (propertize date 'face 'font-lock-string-face))
           (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
           message))
      (git-messenger:format-detail vcs commit-id author message)))
  (defun my/git-messenger:popup-message ()
    "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
    (interactive)
    (let* ((hydra-hint-display-type 'message)
           (vcs (git-messenger:find-vcs))
           (file (buffer-file-name (buffer-base-buffer)))
           (line (line-number-at-pos))
           (commit-info (git-messenger:commit-info-at-line vcs file line))
           (commit-id (car commit-info))
           (author (cdr commit-info))
           (msg (git-messenger:commit-message vcs commit-id))
           (popuped-message (if (git-messenger:show-detail-p commit-id)
                                (my/git-messenger:format-detail vcs commit-id author msg)
                              (cl-case vcs
                                (git msg)
                                (svn (if (string= commit-id "-")
                                         msg
                                       (git-messenger:svn-message msg)))
                                (hg msg)))))
      (setq git-messenger:vcs vcs
            git-messenger:last-message msg
            git-messenger:last-commit-id commit-id)
      (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
      (git-messenger-hydra/body)
      (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
             (let ((buffer-name "*git-messenger*"))
               (posframe-show buffer-name
                              :string (concat (propertize "\n" 'face '(:height 0.3))
                                              popuped-message
                                              "\n"
                                              (propertize "\n" 'face '(:height 0.3)))
                              :left-fringe 8
                              :right-fringe 8
                              :internal-border-width 1
                              :internal-border-color (face-foreground 'default)
                              :background-color (face-background 'tooltip nil t))
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (posframe-hide buffer-name))))
            ((fboundp 'popup-tip)
             (popup-tip popuped-message))
            ((fboundp 'lv-message)
             (lv-message popuped-message)
             (unwind-protect
                 (push (read-event) unread-command-events)
               (lv-delete-window)))
            (t (message "%s" popuped-message)))
      (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
  (advice-add #'git-messenger:popup-close :override #'ignore)
  (advice-add #'git-messenger:popup-message :override #'my/git-messenger:popup-message))

(provide 'init-vc)
;;; init-vc ends here
