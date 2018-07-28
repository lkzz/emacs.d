;;; init-filetree --- a tree layout file explorer for Emacs,such as treemacs or neotree. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(use-package neotree
  :defer t
  :ensure t
  :commands (neotree-change-root
             neotree-quick-look
             neotree-toggle
             neotree-hide
             neotree-enter)
  :init
  (progn
    (kevin/set-leader-keys "ft" 'neotree-toggle)
    (setq neo-create-file-auto-open t
          neo-auto-indent-point nil
          neo-autorefresh t
          neo-smart-open t
          neo-mode-line-type 'none
          neo-window-width 30
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
            "^\\(node_modules\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
            ;; org-mode folders
            "^\\.\\(sync\\|export\\|attach\\)$"
            "~$" "\\.emacs*"
            "^#.*#$")))
  :config
  (progn
    (eval-after-load 'evil
      '(progn
         (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
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

(provide 'init-filetree)
;;; init-filetree ends here
