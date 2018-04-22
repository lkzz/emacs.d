;;; init-git.el --- version contral setup
;;; Commentary:
;;; Code:

(defun git-get-current-file-relative-path ()
  "Get current file relative path."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

(defun git-checkout-current-file ()
  "Git checkout current file."
  (interactive)
  (when (and (buffer-file-name)
             (yes-or-no-p (format "git checkout %s?"
                                  (file-name-nondirectory (buffer-file-name)))))
    (let* ((filename (git-get-current-file-relative-path)))
      (shell-command (concat "git checkout " filename))
      (message "DONE! git checkout %s" filename))))

(defun git-add-current-file ()
  "Git add file of current buffer."
  (interactive)
  (let ((filename))
    (when buffer-file-name
      (setq filename (git-get-current-file-relative-path))
      (shell-command (concat "git add " filename))
      (message "DONE! git add %s" filename))))

;; {{ goto next/previous hunk
(defun my-goto-next-hunk (arg)
  (interactive "p")
  (if (memq major-mode '(diff-mode))
      (diff-hunk-next)
    (forward-line)
    (if (re-search-forward "\\(^<<<<<<<\\|^=======\\|^>>>>>>>\\)" (point-max) t)
        (goto-char (line-beginning-position))
      (forward-line -1)
      (git-gutter:next-hunk arg))))

(defun my-goto-previous-hunk (arg)
  (interactive "p")
  (if (memq major-mode '(diff-mode))
      (diff-hunk-prev)
    (forward-line -1)
    (if (re-search-backward "\\(^>>>>>>>\\|^=======\\|^<<<<<<<\\)" (point-min) t)
        (goto-char (line-beginning-position))
      (forward-line -1)
      (git-gutter:previous-hunk arg))))

(use-package magit
  :ensure t
  :commands (magit-status magit-init magit-file-log magit-blame-mode)
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
  (progn
    (evil-leader/set-key
      "ga" #'git-add-current-file
      "gc" #'git-checkout-current-file
      "gd" (lambda ()
             (interactive)
             (let* ((ffip-diff-backends
                     '(("Show git commit" . (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'")
                                                   (collection (split-string (shell-command-to-string git-cmd) "\n" t))
                                                   (item (ffip-completing-read "git log:" collection)))
                                              (when item
                                                (shell-command-to-string (format "git show %s" (car (split-string item "|" t))))))))))
               (ffip-show-diff 0)))
      "gfd" 'magit-diff-buffer-file
      "gfl" 'magit-log-buffer-file
      "gi"  'magit-init
      "gL"  'magit-list-repositories
      "gm"  'magit-dispatch-popup
      "gs"  'magit-status
      "gS"  'magit-stage-file
      "gU"  'magit-unstage-file))
  :config
  (progn
    ;; display buffer fullframe
    (setq magit-display-buffer-function
          (lambda (buffer)
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
              (get-buffer-window buffer))))
    ))

(use-package evil-magit
  :ensure t
  :after (magit evil))

;; Gitflow externsion for Magit
(use-package magit-gitflow
  :ensure t
  :defer t
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
  :ensure t
  :defer t
  :commands git-messenger:copy-message
  :bind (("C-x v p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

;; ;; Highlighting regions by last updated time
;; (use-package smeargle
;;   :ensure t
;;   :defer t
;;   :bind (("C-x v S" . smeargle)
;;          ("C-x v C" . smeargle-commits)
;;          ("C-x v R" . smeargle-clear)))

;; Walk through git revisions of a file
(use-package git-timemachine
  :ensure t
  :defer t
  :commands (hydra-git-timemachine/body)
  :init
  (progn
    (evil-leader/set-key "gt" #'hydra-git-timemachine/body)
    ;; (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state)
    (defhydra hydra-git-timemachine (:body-pre (unless (bound-and-true-p git-timemachine-mode)
                                                 (call-interactively 'git-timemachine))
                                               :post (git-timemachine-quit)
                                               :color red
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
))

;; Git modes
(use-package gitconfig-mode
  :ensure t
  :defer t
  :mode (("/\\.?git/?config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)
         ("/_gitconfig\\'" . gitconfig-mode))
  :config
  (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :ensure t
  :defer t
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)))

(use-package git-link
  :ensure t
  :defer t
  :init
  (progn
    (evil-leader/set-key "gl" 'git-link-commit))
  :config
  (setq git-link-open-in-browser t))

(use-package smerge
  :ensure nil
  :defer t
  :init
  (progn
    (defhydra hydra-smerge-mode (:foreign-keys run
                                               :hint nil)
      "
[_n_] previous [_p_] next [_a_] keep all [_b_] keep base [_o_] keep other [_c_] keep current [_C_] combine next [_r_] refine [_q_] quit
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("a" smerge-keep-all)
      ("b" smerge-keep-base)
      ("m" smerge-keep-mine)
      ("o" smerge-keep-other)
      ("c" smerge-keep-current)
      ("C" smerge-combine-with-next)
      ("r" smerge-refine)
      ("q" nil :exit t))
    (evil-leader/set-key "gr" #'hydra-smerge-mode/body)
    ))

(provide 'init-git)
;;; init-git ends here
