;;; init-git.el --- version contral setup
;;; Commentary:
;;; Code:

;; TODO: smerge-mode

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("C-c M-g" . magit-file-popup))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  ;; git-commit
  (use-package git-commit)
  (use-package evil-magit)

  ;; Gitflow externsion for Magit
  (use-package magit-gitflow
    :diminish magit-gitflow-mode
    :bind (:map magit-status-mode-map
                ("G" . magit-gitflow-popup))
    :init (add-hook 'magit-mode-hook #'turn-on-magit-gitflow)
    :config
    (magit-define-popup-action 'magit-dispatch-popup
                               ?G "GitFlow" #'magit-gitflow-popup ?!)))

;;; Pop up last commit information of current line
(use-package git-messenger
  :commands git-messenger:copy-message
  :init
  (evil-leader/set-key
    "g M" 'git-messenger:popup-message)
  :bind (("C-x v p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init
  ;; Use magit-show-commit for showing status/diff commands
  (setq git-messenger:use-magit-popup t))

;; Highlighting regions by last updated time
(use-package smeargle
  :bind (("C-x v S" . smeargle)
         ("C-x v C" . smeargle-commits)
         ("C-x v R" . smeargle-clear)))

;; Walk through git revisions of a file
(use-package git-timemachine)

;; Git modes
(use-package gitattributes-mode)
(use-package gitconfig-mode)
(use-package gitignore-mode)


(provide 'init-git)
;;; init-git ends here
