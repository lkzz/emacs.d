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

;; Load newer version of .el and .elc if both are available
(setq load-prefer-newer t)

;;------------------------------------------------------------------------------
;; use straight as package manager
;;------------------------------------------------------------------------------
;; init before load straight
(setq straight-base-dir my-cache-dir
      straight--process-log nil
      straight-cache-autoloads t
      straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-enable-package-integration nil
      straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t
      use-package-always-ensure nil)
;; load straight bootstrap file
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

(setq use-package-enable-imenu-support t
      use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-expand-minimally t
      use-package-compute-statistics nil)
(setq byte-compile-warnings '(cl-functions))
(straight-use-package 'use-package)

(use-package diminish)
(use-package hydra)
(use-package popup)
(use-package posframe)

;; Don't litter emacs directory
(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" my-cache-dir)
        no-littering-var-directory (expand-file-name "var/" my-cache-dir))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package general
  :config
  (general-create-definer my-leader-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix my-leader-key-prefix)
  (general-create-definer my-local-leader-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix my-local-leader-key-prefix))

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
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold #x1000000) ; 16MB
  (gcmh-mode 1))

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
