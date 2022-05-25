;;; core-package.el --- package install config. -*- lexical-binding: t; -*-
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

;; Use straight as package manager
(setq straight-base-dir my-cache-dir
      straight--process-log nil
      straight-cache-autoloads t
      straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-enable-package-integration nil
      straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t
      use-package-always-ensure nil)
;; Load straight bootstrap file
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(setq use-package-enable-imenu-support t
      use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-expand-minimally t
      use-package-compute-statistics nil)
(setq byte-compile-warnings '(cl-functions))
(straight-use-package 'use-package)

(use-package simple
  :straight (:type built-in)
  :hook ((after-init . size-indication-mode) ; 显示百分比进度
         (text-mode . visual-line-mode))
  :init
  (setq line-number-mode t              ; 打开行号显示
        column-number-mode t            ; 打开列号显示
        kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil            ; Move line by visual line
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        column-number-indicator-zero-based nil ; column starts from 1
        kill-do-not-save-duplicates t   ; eliminate duplicates
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again
  ;; 设置visual line fringe bitmap
  (when (and (fboundp 'define-fringe-bitmap) (display-graphic-p))
    (define-fringe-bitmap 'right-curly-arrow
      [#b00000000
       #b01111100
       #b01111100
       #b00001100
       #b00001100
       #b00000000
       #b00000000])
    (define-fringe-bitmap 'left-curly-arrow
      [#b00000000
       #b00110000
       #b00110000
       #b00111110
       #b00111110
       #b00000000
       #b00000000])
    (set-fringe-bitmap-face 'right-curly-arrow 'warning)
    (set-fringe-bitmap-face 'left-curly-arrow 'warning)
    (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))))

(use-package so-long
  :straight (:type built-in)
  :hook (after-init . global-so-long-mode)
  :config
  (setq so-long-threshold 10240)
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; ...and insist that save-place not operate in large/long files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; Text files could possibly be too long too
  (add-to-list 'so-long-target-modes 'text-mode)
  ;; disable some mode that may be unnecessary/expensive for large buffer
  (add-to-list 'so-long-minor-modes 'rainbow-mode)
  (add-to-list 'so-long-minor-modes 'flycheck-mode)
  (add-to-list 'so-long-minor-modes 'eldoc-mode)
  (add-to-list 'so-long-minor-modes 'ws-butler-mode)
  (add-to-list 'so-long-minor-modes 'undo-tree-mode)
  (add-to-list 'so-long-minor-modes 'highlight-numbers-mode)
  (add-to-list 'so-long-minor-modes 'rainbow-delimiters-mode)
  (add-to-list 'so-long-minor-modes 'highlight-indent-guides-mode))

(use-package hydra)
(use-package popup)
(use-package posframe)
(use-package diminish)

;; Buffer index
(use-package imenu
  :hook (imenu-after-jump . recenter))

;; gt next-tab
;; gT prev-tab
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-function 'tab-bar-tab-name-all))

(use-package autoload
  :straight (:type built-in)
  :init
  (defun my/generate-autoload-define (loaddef &rest DIRS)
    (interactive)
    (let ((generated-autoload-file loaddef))
      (when (not (file-exists-p loaddef))
        (with-current-buffer (find-file-noselect generated-autoload-file)
	      (insert ";; generated by function: `my/generate-autoload-define'.")
	      (save-buffer)))
      (apply 'update-directory-autoloads DIRS))))

(when (not (file-exists-p my-autoload-file))
  (my/generate-autoload-define my-autoload-file (concat user-emacs-directory "core/autoload/"))
  (byte-compile-file my-autoload-file)
  (message "generate autoload file: %s done." my-autoload-file))
(load my-autoload-file nil 'nomessage)

;; Don't litter emacs directory
(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" my-cache-dir)
        no-littering-var-directory (expand-file-name "var/" my-cache-dir))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package general
  :config
  (general-create-definer my/global-leader-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix my-global-leader-prefix)
  (general-create-definer my/local-leader-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix my-local-leader-prefix))

(use-package better-jumper
  :init
  (global-set-key [remap evil-jump-forward] #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  ;; xref jump
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (better-jumper-mode +1)
  (setq better-jumper-context 'window
        better-jumper-new-window-behavior 'copy
        better-jumper-add-jump-behavior 'replace
        better-jumper-max-length 100
        better-jumper-use-evil-jump-advice t)
  ;; Auto set mark cmd list
  (dolist (cmd '(lsp-bridge-find-def))
    (advice-add cmd :after #'better-jumper-set-jump)))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package gcmh
  :straight (:host github :repo "emacsmirror/gcmh")
  :hook (my-first-input . gcmh-mode)
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x1000000)) ; 16MB

(use-package which-key
  :diminish which-key-mode "Ⓚ"
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.3
        which-key-compute-remaps t
        which-key-min-display-lines 1
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-sort-uppercase-first nil
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.25
        which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-setup-side-window-bottom)
  (dolist (item '((("SPC" . nil) . ("␣" . nil))
                  (("TAB" . nil) . ("↹" . nil))
                  (("RET" . nil) . ("⏎" . nil))
                  (("DEL" . nil) . ("⌫" . nil))
                  (("<up>" . nil) . ("↑" . nil))
                  (("<down>" . nil) . ("↓" . nil))
                  (("<left>" . nil) . ("←" . nil))
                  (("<right>" . nil) . ("→" . nil))
                  (("deletechar" . nil) . ("⌦" . nil))
                  ;; rename winum-select-window-1 entry to 1..9
                  (("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9"))
                  ;; hide winum-select-window-[2-9] entries
                  ((nil . "winum-select-window-[2-9]") . t)))
    (cl-pushnew item which-key-replacement-alist :test #'equal))
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))

(provide 'core-package)
;;; core-package.el ends here
