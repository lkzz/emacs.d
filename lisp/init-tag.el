;;; init-tag.el --- Initialize tags configurations. -*- lexical-binding: t; -*-
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

(use-package counsel-etags
  :defer t
  :config
  ;; counsel-etags-ignore-directories does NOT support wildcast
  (add-to-list 'counsel-etags-ignore-directories "build_clang")
  ;; counsel-etags-ignore-filenames supports wildcast
  (add-to-list 'counsel-etags-ignore-filenames "TAGS")
  (add-to-list 'counsel-etags-ignore-filenames "*.json")
  ;; (setq tags-file-name "~/Code/gopath/src/go-common/tags")
  )

(use-package bm
  :ensure t
  :after evil
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)
  (defun kevin/add-persistent-bookmark (&rest _)
    (let ((annotation (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (bm-bookmark-add annotation nil nil)))
  :config
  ;;跳转到书签处时，是否将其调整到屏幕中心
  (setq bm-recenter nil)
  ;;书签以何种形式展示
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  ;;书签跳转时，是在当前buffer间跳转还是在所有buffer间跳转
  (setq bm-cycle-all-buffers t)
  ;;是否按照书签的添加顺序进行跳转（默认是从上到下按位置顺序跳转）
  (setq bm-in-lifo-order nil)
  (setq bm-marker 'bm-marker-right)
  (setq bm-repository-size 100)
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)
  ;; where to store persistant files
  (setq bm-repository-file (concat kevin-cache-directory "bm-repository"))
  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)

  ;; 按下ctrl-g时，如果当前光标处有书签，则删除此书签
  (advice-add 'keyboard-quit :before 'bm-bookmark-remove)
  (advice-add 'kevin/goto-definition :before 'kevin/add-persistent-bookmark)
  (advice-add 'bm-previous :before 'kevin/add-persistent-bookmark)

  (define-key evil-normal-state-map "gd" 'kevin/goto-definition)
  (define-key evil-normal-state-map (kbd "C-m") 'bm-toggle)
  (define-key evil-normal-state-map (kbd "C-.") 'bm-next)
  (define-key evil-normal-state-map (kbd "C-,") 'bm-previous)
  (define-key evil-normal-state-map (kbd "C-/") 'bm-remove-all-all-buffers)

  )


(provide 'init-tag)
;;; init-tag.el ends here
