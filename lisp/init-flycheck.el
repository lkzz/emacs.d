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
  :defer t
  :diminish flycheck-mode "ⓕ"
  :hook (prog-mode . global-flycheck-mode)
  :init
  (kevin/set-leader-keys "e" #'flycheck-hydra/body)
  :pretty-hydra
  ((:color red :quit-key "q")
   ("Flycheck"
    (("q" nil "quit" :exit t)
     ("v" flycheck-verify-setup "verify setup" :exit t)
     ("m" flycheck-manual "manual" :exit t))
    "Errors"
    (("l" flycheck-list-errors "list" :exit t)
     ("c" flycheck-buffer "check" :exit t)
     ("n" flycheck-next-error "next")
     ("p" flycheck-previous-error "previous"))
    "Checker"
    (("d" flycheck-disable-checker "disable" :exit t)
     ("s" flycheck-select-checker "select" :exit t)
     ("?" flycheck-describe-checker "describe" :exit t))))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'kevin-flycheck-error-fringe
      (vector #b00000000
              #b00000000
              #b11000011
              #b01100110
              #b00111100
              #b00111000
              #b00111100
              #b01100110
              #b11000011
              #b00000000
              #b00000000))
    (define-fringe-bitmap 'kevin-flycheck-warn-fringe
      (vector #b00000000
              #b00000000
              #b00110000
              #b00110000
              #b00110000
              #b00110000
              #b00110000
              #b00110000
              #b00000000
              #b00000000
              #b00110000
              #b00110000
              #b00000000
              #b00000000))
    (define-fringe-bitmap 'kevin-flycheck-info-fringe
      (vector #b00000000
              #b00000000
              #b00000000
              #b00000000
              #b00011100
              #b00111110
              #b00111110
              #b00111110
              #b00011100
              #b00000000
              #b00000000
              #b00000000
              #b00000000))
    )
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'kevin-flycheck-error-fringe
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'kevin-flycheck-warn-fringe
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'kevin-flycheck-info-fringe
    :fringe-face 'flycheck-fringe-info)
  (setq flycheck-emacs-lisp-check-declare t
        flycheck-display-errors-delay 0.25
        flycheck-indication-mode 'right-fringe
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-highlighting-mode 'symbols
        flycheck-check-syntax-automatically '(save mode-enabled))
  ;; c/c++ mode
  (setq flycheck-gcc-language-standard "c++11")
  (setq flycheck-clang-language-standard "c++11")
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

(use-package flycheck-posframe
  :defer t
  :when (display-graphic-p)
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config  (add-to-list 'flycheck-posframe-inhibit-functions
			#'(lambda () (bound-and-true-p company-backend))))

(use-package flycheck-popup-tip
  :defer t
  :unless (display-graphic-p)
  :after flycheck
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
