;;; init-edit.el --- insert description here -*- lexical-binding: t -*-
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
;;; Code:

;; 自动刷新文件
(use-package autorevert
  :straight (:type built-in)
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

;; 一键删除选择区域
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; Rectangle
(use-package rect
  :straight (:type built-in)
  :bind ("<C-return>" . rectangle-mark-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :straight (:type built-in)
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
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history nil
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before (lambda ()
                                        (if (eq major-mode 'undo-tree-visualizer-mode)
                                            (undo-tree-visualizer-quit)))))

(provide 'init-edit)
;;; init-edit.el ends here
