;;; init-git.el --- version contral setup
;;; Commentary:
;;; Code:

;; TODO: smerge-mode

(use-package magit
  :ensure t
  :defer t
  :bind
  (("C-x g i" . magit-init)
   ("C-x g f" . magit-file-log)
   ("C-x g b" . magit-blame-mode)
   ("C-x g m" . magit-branch-manager)
   ("C-x g c" . magit-branch)
   ("C-x g s" . magit-status)
   ("C-x g r" . magit-reflog)
   ("C-x g t" . magit-tag))
  :config
  (progn
    (set-default 'magit-stage-all-confirm nil)
    (add-hook 'magit-mode-hook 'magit-load-config-extensions)
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows)))

  ;; Gitflow externsion for Magit
  (use-package magit-gitflow
    :ensure t
    :defer t
    :diminish magit-gitflow-mode
    :bind (:map magit-status-mode-map
                ("G" . magit-gitflow-popup))
    :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow)
    :config
    (magit-define-popup-action 'magit-dispatch-popup
                               ?G "GitFlow" #'magit-gitflow-popup ?!)))

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
  :ensure t)

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
