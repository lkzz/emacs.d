;;; init-flycheck.el --- initialize flycheck configurations. -*- lexical-binding: t; -*-
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

(use-package flycheck
  :diminish flycheck-mode "ⓕ"
  :commands (hydra-flycheck/body)
  :hook (prog-mode . global-flycheck-mode)
  :init
  (kevin/declare-prefix "e" "flycheck")
  (kevin/set-leader-keys "ec" #'flycheck-buffer
                         "el" #'flycheck-list-errors
                         "ep" #'flycheck-previous-error
                         "en" #'flycheck-next-error)
  (defhydra hydra-flycheck (:color red
                                   :hint nil)
    "
    ^
    ^Flycheck^        ^Errors^          ^Checker^
    ^────────^────────^──────^──────────^───────^───────────
    _q_ quit          _c_ check         _s_ select
    _v_ verify setup  _n_ next          _d_ disable
    _m_ manual        _p_ previous      _?_ describe
                    _l_ list
    ^^                  ^^                  ^^
    "
    ("q" nil exit: t)
    ("c" flycheck-buffer exit: t)
    ("d" flycheck-disable-checker exit: t)
    ("l" flycheck-list-errors exit: t)
    ("m" flycheck-manual exit: t)
    ("n" flycheck-next-error exit: t)
    ("p" flycheck-previous-error exit: t)
    ("s" flycheck-select-checker exit: t)
    ("v" flycheck-verify-setup exit: t)
    ("?" flycheck-describe-checker exit: t))
  :config
  (setq flycheck-emacs-lisp-check-declare t
        flycheck-indication-mode 'right-fringe
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-highlighting-mode 'symbols
        flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

(use-package flycheck-posframe
  :if (display-graphic-p)
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))

(use-package flycheck-popup-tip
  :unless (display-graphic-p)
  :after flycheck
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
