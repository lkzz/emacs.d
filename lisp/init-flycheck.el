;;; init-flycheck.el --- initialize flycheck configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  Kevin Leung

;;; This file is NOT part of GNU Emacs

;;; License

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; setup flycheck package

;;; Code:

;; Add the following code to emacs startup file.

;; (require 'init-yasnippet)

(use-package flycheck
  :defer t
  :diminish flycheck-mode "ⓕ"
  :commands (hydra-flycheck/body)
  :hook (after-init . global-flycheck-mode)
  :init
  (kevin/declare-prefix "e" "flycheck")
  (kevin/set-leader-keys "ec" #'flycheck-buffer)
  (kevin/set-leader-keys "el" #'flycheck-list-errors)
  (kevin/set-leader-keys "ep" #'flycheck-previous-error)
  (kevin/set-leader-keys "en" #'flycheck-next-error)
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
  (setq flycheck-emacs-lisp-check-declare t)
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-highlighting-mode 'symbols)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

;; Jump to and fix syntax errors via `avy'
(use-package avy-flycheck
  :ensure t
  :after (avy flycheck)
  :init
  (avy-flycheck-setup)
  (kevin/set-leader-keys "eg" #'avy-flycheck-goto-error))

(use-package flycheck-posframe
  :ensure t
  :if (display-graphic-p)
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
