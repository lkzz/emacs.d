;;; init-whitespace.el --- whitespace mode config. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(use-package whitespace
  :defer t
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook outline-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq show-trailing-whitespace t)
  (setq whitespace-line-column fill-column) ;; limit line length
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; only show bad whitespace
  ;; (setq whitespace-style '(face tabs trailing tab-mark))
  (setq whitespace-style
        '(face
          ;; trailing blanks
          trailing
          ;; empty lines at beginning and/or end of buffer
          ;; empty
          ;; line is longer `whitespace-line-column'
          ;; lines-tail
          ;; tab or space at the beginning of the line according to
          ;; `indent-tabs-mode'
          indentation
          ;; show tab as » (see `whitespace-display-mappings')
          tab-mark))

  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)

    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
      "Turn off whitespace mode before showing autocomplete box."
      (if whitespace-mode
          (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))

    (defadvice popup-delete (after my-restore-whitespace activate compile)
      "Restore previous whitespace mode when deleting autocomplete box."
      (if my-prev-whitespace-mode
          (whitespace-mode 1)))))

(use-package whitespace-cleanup-mode
  :defer t
  :diminish whitespace-cleanup-mode
  :init
  (add-hook 'after-init-hook 'global-whitespace-cleanup-mode))

  (provide 'init-whitespace)
;;; init-whitespace ends here
