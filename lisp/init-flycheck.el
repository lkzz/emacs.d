;;; init-flycheck.el --- initialize flycheck
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode "ⓕ"
  :hook (prog-mode . flycheck-mode)
  :init
  (progn
    (setq flycheck-emacs-lisp-check-declare t)
    (setq flycheck-indication-mode 'right-fringe)
    (setq flycheck-emacs-lisp-load-path 'inherit))
  :config
  (defhydra hydra-flycheck (:color blue
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
    ("q" nil)
    ("c" flycheck-buffer)
    ("d" flycheck-disable-checker)
    ("l" flycheck-list-errors :color red)
    ("m" flycheck-manual)
    ("n" flycheck-next-error :color red)
    ("p" flycheck-previous-error :color red)
    ("s" flycheck-select-checker)
    ("v" flycheck-verify-setup)
    ("?" flycheck-describe-checker))
  (evil-leader/set-key "e" 'hydra-flycheck/body)

  ;; Display Flycheck errors in GUI tooltips
  (use-package flycheck-pos-tip
    :ensure t
    :defer t
    :init (flycheck-pos-tip-mode 1)
    :config (setq flycheck-pos-tip-timeout 5))

  ;; Jump to and fix syntax errors via `avy'
  (use-package avy-flycheck
    :ensure t
    :defer t
    :init (avy-flycheck-setup))

  ;; Which colors the mode line according to the Flycheck state of the current buffer
  (use-package flycheck-color-mode-line
    :ensure t
    :defer t
    :init (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
