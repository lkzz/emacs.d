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
                              (setq-local company-dabbrev-code-everywhere t))))
         (before-save . (lambda()
                          (when (or (eq major-mode 'c-mode)
                                    (eq major-mode 'c++-mode)
                                    (eq major-mode 'glsl-mode))
                            (lsp-format-buffer)))))
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
