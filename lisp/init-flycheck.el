;;; init-flycheck.el --- initialize flycheck
;;; Commentary:
;;; Code:

(use-package flycheck
  :defer t
  :diminish flycheck-mode "ⓕ"
  :commands (hydra-flycheck/body)
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-check-declare t)
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  (setq-default flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-display-errors-delay 1.5)
  (kevin/set-leader-keys "fe" #'hydra-flycheck/body)
  ;; customize zenburn theme
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
  :after flycheck
  :init (avy-flycheck-setup))

;; Which colors the mode line according to the Flycheck state of the current buffer
(use-package flycheck-color-mode-line
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

;; Display Flycheck errors in GUI tooltips
(use-package flycheck-posframe
  :ensure t
  :after (posframe flycheck)
  :hook (flycheck-mode . flycheck-posframe-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
