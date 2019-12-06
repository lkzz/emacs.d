;;; init-elpa.el --- elpa config. -*- lexical-binding: t; -*-
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

;;-----------------------------------------------------------------------------
;; enable package manager: straight
;;-----------------------------------------------------------------------------
(setq straight-base-dir user-emacs-directory
      ;; Straight's own emacsmirror mirror is a little smaller and faster.
      straight-recipes-emacsmirror-use-mirror t
      straight-enable-package-integration nil
      straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save)
      straight-vc-git-default-clone-depth 1)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;-----------------------------------------------------------------------------
;; install use-package
;;-----------------------------------------------------------------------------
(straight-use-package 'use-package)
;; Should set before loading `use-package'
(eval-and-compile
  (setq straight-use-package-by-default t)
  ;; (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (require 'use-package))


(use-package diminish)
(use-package bind-key)
(use-package hydra)
(use-package pretty-hydra
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (display-graphic-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(use-package posframe)
(use-package dash)
(use-package dash-functional)
(use-package all-the-icons)
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer kevin/set-leader-keys-for-major-mode
    :states '(normal visual)
    :keymaps 'override ; keybindings that should not be overriden
    :prefix kevin-major-mode-leader-key)
  (general-create-definer kevin/set-leader-keys
    :states '(normal insert emacs visual)
    :prefix kevin-leader-key
    :keymaps 'override ; keybindings that should not be overriden
    :non-normal-prefix kevin-emacs-leader-key))

(use-package which-key
  :diminish which-key-mode "Ⓚ"
  :commands (which-key-add-major-mode-key-based-replacements
              which-key-add-key-based-replacements)
  :hook (after-init . which-key-mode)
  :init
  (defun kevin/declare-prefix (prefix name)
    (let* ((full-prefix (concat kevin-leader-key " " prefix))
           (full-prefix-emacs (concat kevin-emacs-leader-key " " prefix)))
      (which-key-add-key-based-replacements
        full-prefix name
        full-prefix-emacs name)))
  (defun kevin/declare-prefix-for-major-mode (mode prefix name)
    (let* ((full-prefix (concat kevin-major-mode-leader-key " " prefix)))
      (which-key-add-major-mode-key-based-replacements mode full-prefix name)))
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
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
  ;; rename winum-select-window-1 entry to 1..9
  (add-to-list 'which-key-replacement-alist '(("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9")))
  ;; hide winum-select-window-[2-9] entries
  (add-to-list 'which-key-replacement-alist '((nil . "winum-select-window-[2-9]") . t))
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))

(use-package auto-package-update
  :config
  (setq auto-package-update-interval 7
        auto-package-update-delete-old-versions t))


(provide 'init-elpa)
;;; init-elpa.el ends here
