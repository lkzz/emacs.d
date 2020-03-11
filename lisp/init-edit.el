;;; init-edit.el --- insert description here -*- lexical-binding: t -*-
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
;;; Code:

;; 自动刷新文件
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

;; 一键删除选择区域
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; Rectangle
(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :commands (undo-tree-visualize)
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history nil
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-history-directory-alist `(("." . ,(concat kevin-cache-directory "undo-tree-history"))))
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'(lambda ()
                                          (if (eq major-mode 'undo-tree-visualizer-mode)
                                              (undo-tree-visualizer-quit)))))

(use-package multiple-cursors
  :bind (("C-c m t" . mc/mark-all-like-this)
         ("C-c m m" . mc/mark-all-like-this-dwim)
         ("C-c m l" . mc/edit-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m a" . mc/edit-beginnings-of-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m s" . mc/mark-sgml-tag-pair)
         ("C-c m d" . mc/mark-all-like-this-in-defun)))

(provide 'init-edit)
;;; init-edit.el ends here
