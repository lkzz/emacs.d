;;; init-git.el --- version contral setup
;;; Commentary:
;;; Code:

;; TODO: smerge-mode

(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-blame)
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
    (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)))

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

;; Highlighting regions by last updated time
(use-package smeargle
  :ensure t
  :defer t
  :bind (("C-x v S" . smeargle)
         ("C-x v C" . smeargle-commits)
         ("C-x v R" . smeargle-clear)))

;; Walk through git revisions of a file
(use-package git-timemachine
  :defer t
  :ensure t
  :commands (git-timemachine git-timemachine-toggle)
  :init
  ;; Force evil to rehash keybindings for the current state
  (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state)
  (defhydra hydra-git-timemachine (:body-pre (unless (bound-and-true-p git-timemachine-mode)
                                               (call-interactively 'git-timemachine))
                                             :post (git-timemachine-quit)
                                             :color red
                                             :hint nil
                                             )
    "
[_p_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit\n
"
    ("c" git-timemachine-show-current-revision)
    ("g" git-timemachine-show-nth-revision)
    ("p" git-timemachine-show-previous-revision)
    ("n" git-timemachine-show-next-revision)
    ("Y" git-timemachine-kill-revision)
    ("q" nil exit: t)))
(evil-leader/set-key "gt" #'hydra-git-timemachine/body)

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

(provide 'init-git)
;;; init-git ends here
