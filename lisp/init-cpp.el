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

(defun kevin/cxx-mode-setup ()
  (require 'rtags)

  (setq c-default-style "horstmann")
  ;; add horstmann style - copy bsd style
  (c-add-style "horstmann"
               '("bsd"
                 (c-offsets-alist
                  (case-label . +)))
               )  ;; end c-add-style
  (setq c-basic-offset 4)
  ;; don't mix tab and space indents
  (setq indent-tabs-mode nil)
  ;; don't change alignment of C type comments (fixes problem in JEdit)
  (c-set-offset 'c 'c-lineup-dont-change)
  ;; align a continued string under the one it continues
  (c-set-offset 'statement-cont 'c-lineup-string-cont)
  ;; align or indent after an assignment operator
  (c-set-offset 'statement-cont 'c-lineup-math)
  ;; align closing brace/paren with opening brace/paren
  (c-set-offset 'arglist-close 'c-lineup-close-paren)
  (c-set-offset 'brace-list-close 'c-lineup-close-paren)
  ;; align current argument line with opening argument line
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-arglist)
  ;; don't change indent of java 'throws' statement in method declaration
  ;;     and other items after the function argument list
  (c-set-offset 'func-decl-cont 'c-lineup-dont-change)
  ;; always unindent C++ class access labels
  (c-set-offset  'access-label -4)
  ;; set to NOT Indent Namespaces
  (c-set-offset  'namespace-open 0)
  (c-set-offset  'namespace-close 0)
  (c-set-offset  'innamespace 0)
  )

(use-package cc-mode
  :hook ((c-mode c++-mode) . kevin/cxx-mode-setup)
  :bind (:map c++-mode-map
              ("C-c C-o" . ff-find-other-file))
  :init
  (setq cc-other-file-alist
        '(("\\.c\\'"   (".h"))
          ("\\.C\\'"   (".h" ".hpp" ".hxx"))
          ("\\.cc\\'"  (".h" ".hpp" ".hxx"))
          ("\\.cpp\\'" (".h" ".hpp" ".hxx"))
          ("\\.cxx\\'" (".h" ".hpp" ".hxx"))
          ("\\.tpp\\'" (".h" ".hpp" ".hxx"))
          ("\\.tcc\\'" (".h" ".hpp" ".hxx"))
          ("\\.h\\'"   (".tpp" ".cpp" ".cxx" ".tcc" ".cc" ".C" ".c" ".hxx" ".hpp"))
          ("\\.hpp\\'" (".tpp" ".cpp" ".cxx" ".tcc" ".cc" ".C" ".c" ".h"))
          ("\\.hxx\\'" (".tpp" ".cpp" ".cxx" ".tcc" ".cc" ".C" ".c" ".h"))))
  (setq ff-search-directories
        '("." "../src" "../include"))
  )

(use-package c++-mode
  :ensure nil
  :mode ("\\.h|\\.cpp" . c++-mode))

(use-package company-c-headers
  :after company
  :config
  (kevin/add-company-backend :backend company-c-headers :mode c++-mode))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook ((c-mode c++-mode) . modern-c++-font-lock-mode))
;;--------------------------------common----------------------------------------

;;--------------------------------rtag------------------------------------------
(use-package rtags
  :defer t ; require before c++-mode
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
    :disabled
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
  :if (eq kevin-c++-backend 'irony)
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

;; -----------------------------------cmake--------------------------------------
(use-package cmake-mode
  :mode (("CMakeLists\\.txt$" . cmake-mode)
         ("\\.cmake$'" . cmake-mode))
  :config
  (setq cmake-tab-width 4))

(use-package company-cmake
  :load-path "vendor"
  :after (company cmake-mode)
  :config
  (kevin/add-company-backend :backend company-cmake :mode cmake-mode))

(use-package cmake-font-lock
  :hook (cmake-mode . font-lock-refresh-defaults))
;; -----------------------------------cmake--------------------------------------

;;------------------------ code format -----------------------------------------
(defvar astyle-config-file (expand-file-name "vendor/astyle_config.ini" user-emacs-directory)
  "The location of the astyle configuration file.")
(defvar astyle-command (format "astyle --options=%s" astyle-config-file)
  "Astyle format command.")

(defun kevin/astyle-format-region-or-buffer ()
  "Format the current region or buffer with astyle command"
  (interactive)
  (let ((current-pos (point)))
    (if (region-active-p)
        (shell-command-on-region (region-beginning) (region-end) astyle-command nil t)
      (shell-command-on-region (point-min) (point-max) astyle-command nil t))
    (goto-char current-pos)
    (if mark-active (deactivate-mark))))

(defun kevin/clang-format-region-or-buffer ()
  "Format the current region or buffer with clang-format.
if .clang-format exists in the projectile root, Otherwise, use google style by default"
  (interactive)
  (save-excursion
    (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
      (setq clang-format-style-option "file"))
    (if (region-active-p)
        (clang-format-region (region-beginning) (region-end))
      (clang-format-buffer))))

(defun kevin/c++-format-on-save ()
  (if (eq kevin-c++-format-tool 'clang-format)
      (add-hook 'before-save-hook #'kevin/clang-format-region-or-buffer nil t)
    (add-hook 'before-save-hook #'kevin/astyle-format-region-or-buffer nil t)))

(add-hook 'c++-mode-hook 'kevin/c++-format-on-save)

(use-package clang-format
  :if (eq kevin-c++-format-tool 'clang-format)
  :commands (clang-format-buffer clang-format-region)
  :config
  (setq clang-format-style-option "google"))

;;------------------------ code format -----------------------------------------

(provide 'init-cpp)
;;; init-cpp.el ends here
