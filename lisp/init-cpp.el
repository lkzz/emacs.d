;;; init-cpp.el --- cpp config. -*- lexical-binding: t; -*-
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

(defvar c++-backend 'irony
  "Enable `ycmd or `irony support")

(defvar c++-enable-google-style nil
  "If non-nil `google-set-c-style' will be added as as `c++-mode-hook'.")

(defvar c++-enable-rtags-completion t
  "If `nil', RTags completion is disabled when the RTags backend is enabled.")

;;--------------------------------common----------------------------------------
(defun kevin/c++-mode-setup ()
  (setq c-basic-offset 4
        tab-width c-basic-offset
        c-default-style "java")

  ;; config find-file.el
  (setq cc-other-file-alist
        '(("\\.c"   (".h"))
          ("\\.cpp"   (".h"))
          ("\\.h"   (".c"".cpp"))))
  (setq ff-search-directories
        '("." "../src" "../include"))
  (define-key c++-mode-map (kbd "C-c C-o") 'ff-find-other-file))
(add-hook 'c++-mode-hook 'kevin/c++-mode-setup)

(use-package cc-mode
  :ensure nil
  :diminish abbrev-mode
  :mode ("\\.h\\'" . c++-mode))

(use-package company-c-headers
  :after company
  :config
  (kevin/add-company-backend :backend company-c-headers :mode c++-mode))

(use-package google-c-style
  :if c++-enable-google-style
  :config
  (add-hook 'c++-mode-hook 'google-set-c-style))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :config
  (add-hook 'c++-mode-hook 'modern-c++-font-lock-mode))
;;--------------------------------common----------------------------------------

;;--------------------------------rtag------------------------------------------
(use-package rtags
  :if c++-enable-rtags-completion
  :load-path "vendor/rtags"
  :config
  (setq rtags-autostart-diagnostics t)
  (add-hook 'rtags-jump-hook 'evil-set-jump)
  (rtags-diagnostics)
  ;; key bindings
  (kevin/set-leader-keys-for-major-mode
    :keymaps 'c++-mode-map
    "gg" 'rtags-find-symbol-at-point
    "fr" 'rtags-find-all-references-at-point
    "fR" 'rtags-find-references-at-point
    "ff" 'rtags-find-file
    "g[" 'rtags-location-stack-back
    "g]" 'rtags-location-stack-forward
    "g>" 'rtags-find-symbol
    "g<" 'rtags-find-references
    "gB" 'rtags-show-rtags-buffer
    "gd" 'rtags-print-dependencies
    "gD" 'rtags-diagnostics
    "ge" 'rtags-reparse-file
    "gE" 'rtags-preprocess-file
    "gF" 'rtags-fixit
    "gG" 'rtags-guess-function-at-point
    "gh" 'rtags-print-class-hierarchy
    "gI" 'rtags-imenu
    "gL" 'rtags-copy-and-print-current-location
    "gM" 'rtags-symbol-info
    "gO" 'rtags-goto-offset
    "gp" 'rtags-set-current-project
    "rn" 'rtags-rename-symbol
    "gs" 'rtags-print-source-arguments
    "gS" 'rtags-display-summary
    "gT" 'rtags-taglist
    "gv" 'rtags-find-virtuals-at-point
    "gV" 'rtags-print-enum-value-at-point
    "gX" 'rtags-fix-fixit-at-point
    "gY" 'rtags-cycle-through-diagnostics)

  (use-package company-rtags
    :load-path "vendor/rtags"
    :init
    (setq rtags-completions-enabled t)
    (kevin/add-company-backend :backend company-rtags :mode c++-mode))

  (use-package ivy-rtags
    :load-path "vendor/rtags"
    :init
    (setq rtags-display-result-backend 'ivy))

  )
;;--------------------------------rtag------------------------------------------

;;-------------------------------------irony------------------------------------
(defun kevin/irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

(use-package irony
  :if (eq c++-backend 'irony)
  :diminish irony-mode "ⓘ"
  :after (company flycheck)
  :hook (c++-mode . irony-mode)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'kevin/irony-mode-hook)
  (setq irony-additional-clang-options (append '("-std=c++11") irony-additional-clang-options))
  (use-package irony-eldoc
    :config
    (add-hook 'irony-mode-hook 'irony-eldoc))

  ;; Use company-irony as company mode backend.
  (use-package company-irony
    :config
    (kevin/add-company-backend :backend company-irony :mode c++-mode))

  ;; Use flycheck-irony in CC mode.
  (use-package flycheck-irony
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  )

;;------------------------------------ycmd---------------------------------------
(use-package ycmd
  :if (eq c++-backend 'ycmd)
  :config
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (setq url-show-status nil)
  (setq request-message-level -1)
  (kevin/define-jump-handlers c++-mode (ycmd-goto :async t))
  (set-variable 'ycmd-global-config "/home/kevin/workspace/boss/.ycm_extra_conf.py")
  (set-variable 'ycmd-server-command `("python3" ,(file-truename "~/workspace/third_party/ycmd/ycmd")))

  (use-package company-ycmd
    :after company
    :config
    (kevin/add-company-backends :backend company-ycmd :mode c++-mode))

  (use-package flycheck-ycmd
    :after (flycheck ycmd)
    :config
    (flycheck-ycmd-setup)
    (when (not (display-graphic-p))
      (setq flycheck-indication-mode nil)))

  (use-package ycmd-eldoc
    :disabled
    :if (eq c++-backend 'ycmd)
    :init
    (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))
  )

;;------------------------------------ycmd---------------------------------------

;; -----------------------------------cmake--------------------------------------
(use-package cmake-mode
  :mode (("CMakeLists\\.txt$" . cmake-mode)
         ("\\.cmake$'" . cmake-mode))
  :config
  (setq cmake-tab-width 4))

(use-package company-cmake
  :disabled
  :after (company cmake-mode)
  :load-path "vendor"
  :config
  (kevin/add-company-backends :backend company-cmake :mode cmake-mode)
  )

(use-package cmake-font-lock
  :hook (cmake-mode . font-lock-refresh-defaults))
;; -----------------------------------cmake--------------------------------------

;;------------------------ code format -----------------------------------------
(defvar astyle-config-file (expand-file-name "vendor/astyle_config.ini" user-emacs-directory)
  "The location of the astyle configuration file.")
(defvar astyle-command (format "astyle --options=%s" astyle-config-file)
  "Astyle format command.")

(defun kevin/astyle-format-buffer ()
  "Run astyle command on current buffer."
  (interactive (if mark-active
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))
                 ))
  (save-restriction
    (shell-command-on-region start end
                             astyle-command
                             (current-buffer) t
                             (get-buffer-create "*Astyle Errors*") t))
  )

(add-hook 'before-save-hook '(lambda ()
                               (when (eq major-mode 'c++-mode)
                                 (kevin/astyle-format-buffer))))
;;------------------------ code format -----------------------------------------


(provide 'init-cpp)
;;; init-cpp.el ends here
