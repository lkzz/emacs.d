;;; init-filetree --- a tree layout file explorer for Emacs,such as treemacs or neotree
;;; Commentary:
;;; Code:

(use-package all-the-icons)

(use-package neotree
  :defer t
  :commands (neotree-change-root
             neotree-quick-look
             neotree-toggle
             neotree-hide
             neotree-enter)
  :init
  (progn
    (evil-leader/set-key
      "ft" 'neotree-toggle)
    (setq neo-create-file-auto-open t
          neo-auto-indent-point nil
          neo-autorefresh nil
          neo-mode-line-type 'none
          neo-window-width 25
          neo-show-updir-line nil
          neo-theme (if (display-graphic-p) 'icons 'arrow)
          neo-banner-message nil
          neo-confirm-create-file #'off-p
          neo-confirm-create-directory #'off-p
          neo-show-hidden-files nil
          neo-keymap-style 'concise
          neo-hidden-regexp-list
          '(;; vcs folders
            "^\\.\\(DS_store\\|git\\|gitignore\\|hg\\|svn\\)$"
            ;; compiled files
            "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
            ;; generated files, caches or local pkgs
            "^\\(node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
            ;; org-mode folders
            "^\\.\\(sync\\|export\\|attach\\)$"
            "~$" "\\.emacs*"
            "^#.*#$"))
    )
  :config
  (progn
    (eval-after-load 'evil
      '(progn
         (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
         (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
         (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
         (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
         (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-select-up-node)
         (evil-define-key 'normal neotree-mode-map (kbd "l") 'neotree-change-root)
         (evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-create-node)
         (evil-define-key 'normal neotree-mode-map (kbd "C") 'neotree-copy-node)
         (evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node)
         (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
         (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-rename-node)
         (evil-define-key 'normal neotree-mode-map (kbd "s") 'neotree-hidden-file-toggle)
         ))))

;; (use-package treemacs
;;   :after evil
;;   :init
;;   (progn
;;     (evil-leader/set-key
;;       "ft" #'treemacs-toggle
;;       "fT" #'treemacs
;;       "fB" #'treemacs-bookmark
;;       "f C-t" #'treemacs-find-file))
;;   :config
;;   (progn
;;     (use-package treemacs-evil
;;       :demand t)
;;     (setq treemacs-change-root-without-asking nil
;;           treemacs-collapse-dirs              (if (executable-find "python") 3 0)
;;           treemacs-file-event-delay           5000
;;           treemacs-follow-after-init          t
;;           treemacs-goto-tag-strategy          'refetch-index
;;           treemacs-indentation                2
;;           treemacs-indentation-string         " "
;;           treemacs-is-never-other-window      t
;;           treemacs-never-persist              nil
;;           treemacs-no-png-images              nil
;;           treemacs-recenter-after-file-follow nil
;;           treemacs-recenter-after-tag-follow  nil
;;           treemacs-show-hidden-files          t
;;           treemacs-silent-filewatch           t
;;           treemacs-silent-refresh             nil
;;           treemacs-sorting                    'alphabetic-desc
;;           treemacs-tag-follow-cleanup         t
;;           treemacs-tag-follow-delay           1.5
;;           treemacs-width                      35)
;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-git-mode 'extended)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null (executable-find "python3"))))
;;       (`(t . t)
;;        (treemacs-git-mode 'extended))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple)))
;;     )
;;   :bind
;;   (:map global-map
;;         ([f8]         . treemacs-toggle)
;;         ("M-0"        . treemacs-select-window)
;;         ("C-c 1"      . treemacs-delete-other-windows)
;;         ("C-c ft"     . treemacs-toggle)
;;         ("C-c fT"     . treemacs)
;;         ("C-c fB"     . treemacs-bookmark)
;;         ("C-c f C-t"  . treemacs-find-file)
;;         ("C-c f M-t"  . treemacs-find-tag)))

(provide 'init-filetree)
;;; init-filetree ends here
