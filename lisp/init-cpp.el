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

(defvar astyle-config-file (expand-file-name "vendor/astyle_config.ini" user-emacs-directory)
  "The location of the astyle configuration file.")
(defvar astyle-command (format "astyle --options=%s" astyle-config-file)
  "Astyle format command.")

(defun kevin/astyle-format-region (start end)
  "Run astyle command on region."
  (interactive "r")
  (shell-command-on-region start end astyle-command nil t (get-buffer-create "*Messages*") nil))

(defun kevin/astyle-format-buffer ()
  "Run astyle command on current buffer."
  (interactive)
  (let ((current-pos (point)))
    (kevin/astyle-format-region (point-min) (point-max))
    (goto-char current-pos)
    ;; fix config bug with function: kevin/auto-save-buffer
    (if mark-active (deactivate-mark))))

(add-hook 'before-save-hook '(lambda()
                               (when (or (eq major-mode 'c-mode)
                                         (eq major-mode 'c++-mode))
                                 (kevin/astyle-format-buffer))))

(use-package cc-mode
  :ensure nil
  :diminish abbrev-mode
  :mode ("\\.h\\'" . c++-mode))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package irony
  :diminish irony-mode "ⓘ"
  :after company
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook '(lambda ()
                                (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
                                (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))))

(use-package company-irony-c-headers
  :after company)

(use-package irony-eldoc
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

;; Use company-irony as company mode backend.
(use-package company-irony
  :after company)

(use-package rtags
  :load-path "vendor/rtags"
  :hook (c++-mode . rtags-start-process-unless-running)
  :config
  (setq rtags-path "/usr/local/bin/"
        rtags-completions-enabled t
        rtags-autostart-diagnostics t
        rtags-display-result-backend 'ivy)
  (rtags-diagnostics)
  (rtags-enable-standard-keybindings)
  ;; keybindings
  (kevin/define-jump-handlers c++-mode rtags-find-symbol-at-point)
  (kevin/declare-prefix-for-major-mode 'c++-mode "f" "reference")
  (kevin/set-leader-keys-for-major-mode
    :keymaps 'c-mode-base-map
    "fs" 'rtags-find-symbol
    "fd" 'rtags-symbol-type
    "gg" 'rtags-find-symbol-at-point
    "ff" 'rtags-find-file
    "fr" 'rtags-find-references-at-point
    "fR" 'rtags-find-references
    "fm" 'rtags-imenu)
  (kevin/declare-prefix-for-major-mode 'c++-mode "i" "import")
  (kevin/set-leader-keys-for-major-mode
    :keymaps 'c-mode-base-map
    "ia" 'rtags-include-file
    "is" 'rtags-get-include-file-for-symbol)
  (kevin/declare-prefix-for-major-mode 'c++-mode "r" "refactoring")
  (kevin/set-leader-keys-for-major-mode
    :keymaps 'c++-mode-map
    "rn" 'rtags-rename-symbol))

(use-package ivy-rtags
  :load-path "vendor/rtags"
  :config
  (setq rtags-display-result-backend 'ivy))

(use-package flycheck-rtags
  :load-path "vendor/rtags"
  :if (not (eq system-type 'ms-dos))
  :config
  (defun c++-mode-rtags-hook ()
    (interactive)
    (flycheck-select-checker 'rtags))
  (add-hook 'c++-mode-hook #'c++-mode-rtags-hook))

(use-package company-rtags
  :load-path "vendor/rtags"
  :after (company rtags))

;; Use flycheck-irony in CC mode.
(use-package flycheck-irony
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(defvar kevin/c++-mode-backends '(company-files
                                  company-irony-c-headers
                                  company-irony
                                  company-rtags
                                  company-dabbrev-code)
  "Company backends to be used in CC mode.")

(defun kevin/c++-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (setq c-basic-offset 4
        tab-width 4
        c-default-style "java")
  (setq-local company-backends kevin/c++-mode-backends))
(add-hook 'c++-mode-hook 'kevin/c++-mode-setup)

(use-package cmake-mode
  :mode (("CMakeLists\\.txt$" . cmake-mode)
         ("\\.cmake$'" . cmake-mode))
  :config
  (setq cmake-tab-width 4))

(use-package cmake-ide
  :config
  (cmake-ide-setup))

(use-package company-cmake
  :after (company cmake-mode)
  :load-path "vendor"
  :config
  (setq-local company-idle-delay nil)
  (setq-local company-dabbrev-code-everywhere t)
  (setq-local company-backends '(company-cmake
                                 company-capf
                                 company-files)))

(use-package cmake-font-lock
  :hook (cmake-mode . font-lock-refresh-defaults))

(provide 'init-cpp)
;;; init-cpp.el ends here
