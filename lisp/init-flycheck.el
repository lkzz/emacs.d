;;; init-flycheck.el --- initialize flycheck configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2020  Kevin Leung
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

(use-package flycheck
  :diminish flycheck-mode "ⓕ"
  :commands flycheck-redefine-standard-error-levels
  :hook (prog-mode . global-flycheck-mode)
  :general
  (kevin/space-key-define
    "e" '(nil :which-key "Errors")
    "e l" 'flycheck-list-errors
    "e n" 'flycheck-next-error
    "e p" 'flycheck-previous-error)
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-display-errors-delay 0.25
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode (if (display-graphic-p)
                                     'right-fringe
                                   'right-margin)
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-gcc-language-standard "c++11"
        flycheck-clang-language-standard "c++11")
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  :config
  ;; Prettify indication styles
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow)
  (if (display-graphic-p)
      (use-package flycheck-posframe
        :custom-face (flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
        :hook (flycheck-mode . flycheck-posframe-mode)
        :init (setq flycheck-posframe-border-width 1
                    flycheck-posframe-warning-prefix "⚠ "
                    flycheck-posframe-error-prefix "✕ "
                    flycheck-posframe-inhibit-functions '((lambda (&rest _) (bound-and-true-p company-backend))))
        (with-eval-after-load 'evil
          ;; Don't display popups while in insert or replace mode, as it can affect
          ;; the cursor's position or cause disruptive input delays.
          (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
          (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
