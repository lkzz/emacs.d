;;; init-flycheck.el --- initialize flycheck
;;; Commentary:
;;; Code:

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
  (setq flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  (setq flycheck-emacs-lisp-check-declare t)
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-highlighting-mode 'symbols)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (setq-default flycheck-display-errors-delay 1.5)
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
    ("?" flycheck-describe-checker exit: t)))

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
