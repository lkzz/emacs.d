;;; init-better-default.el --- 一些常用的琐碎的配置，应该在Init.el的最后加载
;;; Commentary:

;;; Code:

;; Personal information
(setq user-full-name "kevin leung")
(setq user-mail-address "kevin.scnu@gmail.com")

(setq select-enable-primary t  ;; 复制粘贴
      select-enable-clipboard t)


(setq-default indent-tabs-mode t ;; 用空格替代TAB
              default-tab-width 4 ;; 定义TAB的宽度为4个空格.
              fill-column 80 ;; 设置列宽度
              global-auto-revert-mode t ;; 自动刷新文件
              buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode nil
              line-spacing 0
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              truncate-lines nil
              truncate-partial-width-windows nil
              visible-bell nil)
;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

;; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

(require-package 'hungry-delete)
(global-hungry-delete-mode t)

(define-key global-map (kbd "RET") 'newline-and-indent)


(defun kevin/prog-mode-hook ()
  "Custom config used in programming mode."
  ;; turn off 'nlinum-mode when there are more than 5000 lines
  (if (buffer-too-big-p) (nlinum-mode -1))
  ;; show trailing spaces in a programming mode
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'kevin/prog-mode-hook)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "saveplace"))


(setq make-backup-files nil)      ;; 关闭备份功能
(setq-default auto-save-mode nil) ;;关闭自动保存模式
(setq auto-save-default nil)      ;;不生成 #filename# 临时文件

;; 关闭lockfile,NOTE:有风险，建议开启
(setq create-lockfiles nil)


(provide 'init-better-default)
;;; init-better-default.el ends here
