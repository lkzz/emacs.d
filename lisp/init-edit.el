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
  :general ("M-;" 'comment-dwim-2))

;; Rectangle
(use-package rect
  :ensure nil
  :general ("<C-return>" 'rectangle-mark-mode))

(use-package expand-region
  :general ("C-=" 'er/expand-region))

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
  :init
  (setq undo-tree-auto-save-history nil
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before (lambda ()
                                        (if (eq major-mode 'undo-tree-visualizer-mode)
                                            (undo-tree-visualizer-quit)))))

(use-package evil-multiedit
  :config
  ;; Highlights all matches of the selection in the buffer.
  (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
  ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
  ;; incrementally add the next unmatched match.
  (define-key evil-normal-state-map (kbd "s-d") 'evil-multiedit-match-and-next)
  ;; Match selected region.
  (define-key evil-visual-state-map (kbd "s-d") 'evil-multiedit-match-and-next)
  ;; Same as M-d but in reverse.
  (define-key evil-normal-state-map (kbd "s-D") 'evil-multiedit-match-and-prev)
  (define-key evil-visual-state-map (kbd "s-D") 'evil-multiedit-match-and-prev)
  ;; RET will toggle the region under the cursor
  (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
  ;; ...and in visual mode, RET will disable all fields outside the selected region
  (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
  ;; For moving between edit regions
  (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
  (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
  (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
  (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev))

(provide 'init-edit)
;;; init-edit.el ends here
