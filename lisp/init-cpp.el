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


(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

(defun kevin/c++-mode-setup ()
  (setq c-basic-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  ;; syntax-highlight aggressively
  ;; (setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0)

                                        ;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  ;; indent
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
  ;;   void fn() // press ENTER here, zero means no indentation
  (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0))

(add-hook 'c-mode-common-hook 'kevin/c++-mode-setup)

(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package company-c-headers
  :after company
  :init (cl-pushnew 'company-c-headers company-backends))

(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

(use-package ccls
  :defer t
  :hook ((c-mode-common . (lambda()
                            (progn
                              (require 'ccls)
                              (condition-case nil
                                  (lsp)
                                (user-error nil))
                              (setq-local company-idle-delay 0.3)
                              (setq-local company-dabbrev-code-everywhere t)))))
  :init
  (setq ccls-args '("--log-file=/tmp/ccls.log")
        ccls-initialization-options '(:index
                                      (:comments 2)
                                      :completion
                                      (:detailedLabel t))))
(use-package company-cmake
  :after (company cmake-mode)
  :load-path "vendor"
  :config
  (setq-local company-idle-delay nil)
  (setq-local company-dabbrev-code-everywhere t)
  (setq-local company-backends '(company-cmake
                                 company-capf
                                 company-files))
  )

(use-package cmake-mode
  :mode (("CMakeLists\\.txt$" . cmake-mode)
         ("\\.cmake$'" . cmake-mode))
  :config
  (setq cmake-tab-width 4))

;; (use-package cmake-font-lock
;;   :hook (cmake-mode . (lambda()
;;                         (progn
;;                           (cmake-font-lock-activate)
;;                           (font-lock-refresh-defaults)))))
(use-package cmake-font-lock
  :hook (cmake-mode . font-lock-refresh-defaults))

(provide 'init-cpp)
;;; init-cpp.el ends here
