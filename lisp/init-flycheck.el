;;; init-flycheck.el --- initialize flycheck configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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
  (my-space-leader-def
    "e" '(nil :wk "error")
    "e l" 'flycheck-list-errors
    "e n" 'flycheck-next-error
    "e p" 'flycheck-previous-error)
  :init
  (setq flycheck-global-modes '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                                    org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-display-errors-delay 0.25
        flycheck-highlighting-mode 'symbols
        flycheck-indication-mode (if (display-graphic-p) 'right-fringe 'right-margin)
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  :config
  ;; Prettify indication styles
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  (use-package flycheck-posframe
    :if (display-graphic-p)
    :custom-face
    (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
    (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
    (flycheck-posframe-background-face ((t (:inherit tooltip))))
    (flycheck-posframe-border-face ((t (:inherit font-lock-comment-face))))
    :hook (flycheck-mode . kevin/maybe-flycheck-posframe-mode)
    :init
    (setq flycheck-posframe-border-width 1)
    :config
    (setq flycheck-posframe-warning-prefix "! "
          flycheck-posframe-info-prefix "··· "
          flycheck-posframe-error-prefix "X ")
    (with-eval-after-load 'company
      (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))
    (with-eval-after-load 'evil-mode
      (add-hook 'flycheck-posframe-inhibit-functions #'evil-insert-state-p)
      (add-hook 'flycheck-posframe-inhibit-functions #'evil-replace-state-p)))

  (use-package flycheck-popup-tip
    :unless (display-graphic-p)
    :hook (flycheck-mode . kevin/maybe-flycheck-posframe-mode)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
