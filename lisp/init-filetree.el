;;; init-filetree --- a tree layout file explorer for Emacs,such as treemacs or neotree. -*- lexical-binding: t; -*-
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

(use-package neotree
  :defer t
  :init
  (setq neo-create-file-auto-open t
        neo-auto-indent-point nil
        neo-autorefresh t
        neo-smart-open t
        neo-mode-line-type 'none
        neo-window-width 28
        neo-show-updir-line nil
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files t
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
          ;; ignore bazel file
          "^bazel*"
          "^#.*#$"))
  :config
  (general-evil-define-key 'normal neotree-mode-map
    "RET" 'neotree-enter
    "TAB" 'neotree-enter
    "q" 'neotree-hide
    "h" 'neotree-select-up-node
    "l" 'neotree-change-root
    "a" 'neotree-create-node
    "C" 'neotree-copy-node
    "d" 'neotree-delete-node
    "g" 'neotree-refresh
    "r" 'neotree-rename-node
    "th" 'neotree-hidden-file-toggle))

(provide 'init-filetree)
;;; init-filetree ends here
