;;; init-flycheck.el --- initialize flycheck
;;; Commentary:
;;; Code:

(use-package flycheck
  :defer t
  :diminish flycheck-mode "ⓕ"
  :commands (hydra-flycheck/body)
  :hook (prog-mode . flycheck-mode)
  :init
  (progn
    (setq flycheck-emacs-lisp-check-declare t)
    (setq flycheck-indication-mode 'right-fringe)
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (kevin/set-leader-keys "fe" #'hydra-flycheck/body)
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
    ;; (custom-set-faces
    ;; '(flycheck-warning ((t (:underline (:color foreground-color :style wave))))))
    )

  ;; Jump to and fix syntax errors via `avy'
  (use-package avy-flycheck
    :defer t
    :init (avy-flycheck-setup))

  ;; Which colors the mode line according to the Flycheck state of the current buffer
  (use-package flycheck-color-mode-line
    :defer t
    :init (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; Display Flycheck errors in GUI tooltips
(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
