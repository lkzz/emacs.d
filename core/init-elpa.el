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

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;;; Fire up package.el
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my initl!
      package--init-file-ensured t)
(package-initialize)
;; 当el文件比elc文件新的时候,则加载el,即尽量Load最新文件文件
(setq load-prefer-newer t)

;;-----------------------------------------------------------------------------
;; install use-package
;;-----------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(use-package diminish)
(use-package bind-map)
(use-package bind-key)
(use-package hydra)
(use-package posframe)
(use-package dash)
(use-package dash-functional)
(use-package all-the-icons)
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer kevin/comma-leader-keys-add
    :prefix kevin-major-mode-leader-key
    :keymaps 'override
    :states '(normal visual motion))
  (general-create-definer kevin/evil-leader-key
    :prefix kevin-leader-key
    :states '(normal visual motion)
    :keymaps 'override)
  (general-create-definer kevin/emacs-leader-key
    :prefix kevin-emacs-leader-key
    :keymaps 'override)
  (defmacro kevin/set-leader-keys (&rest args)
    "Define for both default leader and global leader."
    (declare (indent defun))
    `(progn
       (kevin/evil-leader-key ,@args)
       (kevin/emacs-leader-key ,@args)))
  )

(use-package which-key
  :diminish which-key-mode "Ⓚ"
  :commands (which-key-add-major-mode-key-based-replacements
              which-key-add-key-based-replacements)
  :hook (after-init . which-key-mode)
  :init
  (defun kevin/declare-prefix (prefix name)
    "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command."
    (let* ((full-prefix (concat kevin-leader-key " " prefix))
           (full-prefix-emacs (concat kevin-emacs-leader-key " " prefix)))
      ;; define the prefix command only if it does not already exist
      (which-key-add-key-based-replacements
        full-prefix name
        full-prefix-emacs name)))
  :config
  (setq which-key-idle-delay 0.3
        which-key-min-display-lines 1
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-sort-uppercase-first nil
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.25
        which-key-allow-imprecise-window-fit t ; performance
        which-key-sort-order #'which-key-prefix-then-key-order)
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))


(provide 'init-elpa)
;;; init-elpa.el ends here
