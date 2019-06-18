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

(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(use-package irony
  :diminish irony-mode "Ⓘⓘ"
  :init
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook '(lambda ()
                                (define-key irony-mode-map [remap completion-at-point]
                                  'irony-completion-at-point-async)
                                (define-key irony-mode-map [remap complete-symbol]
                                  'irony-completion-at-point-async))))

(use-package irony-eldoc
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

;; Use company-irony as company mode backend.
(use-package company-irony
  :after (company irony)
  :commands (company-irony))

(use-package company-c-headers
  :after company
  :commands (company-c-headers)
  :config
  (setq company-c-headers-path-user '("." "./include")))

;; Use flycheck-irony in CC mode.
(use-package flycheck-irony
  :commands (flycheck-irony-setup)
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(defvar kevin/cc-mode-backends '(company-files
                                 company-c-headers
                                 company-irony
                                 company-dabbrev-code)
  "Company backends to be used in CC mode.")

(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

(defun kevin/c++-mode-setup ()
  (setq c-basic-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  ;;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  (when (derived-mode-p 'c-mode 'c++-mode)
    (run-hooks 'prog-mode-hook) ; Run prog-mode hook since cc-mode does not derives from it.
    (setq-local company-backends kevin/cc-mode-backends)
    (irony-mode 1)))

(add-hook 'c-mode-common-hook 'kevin/c++-mode-setup)

(use-package cmake-mode
  :mode (("CMakeLists\\.txt$" . cmake-mode)
         ("\\.cmake$'" . cmake-mode))
  :config
  (setq cmake-tab-width 4))

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
