;;; init-modeline.el --- init spaceline. -*- lexical-binding: t; -*-
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

(defun kevin/maybe-alltheicon (&rest args)
  "Display octicon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-alltheicon args)))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  ;; let spaceline handle auzu info in modeline
  (setq anzu-cons-mode-line-p nil)
  (setq powerline-default-separator 'arrow)
  ;; get the nice-looking unicode numbers of window-numbering-mode
  (setq spaceline-window-numbers-unicode t)
  (setq spaceline-workspace-numbers-unicode t)
  ;; To get the mode-line highlight to change color depending on the evil state
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  ;; turn off buffer size info segment
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-remote-host-off)
  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-flycheck-info-off)
  (spaceline-toggle-selection-info-on)
  (spaceline-toggle-input-method-on)
  (spaceline-toggle-buffer-encoding-abbrev-on)
  ;; configure the separator between the minor modes
  (setq spaceline-minor-modes-separator "")
  ;; define version control segment
  (spaceline-define-segment version-control
                            "Version control information."
                            (when vc-mode
                              (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
                                (powerline-raw
                                 (concat
                                  (kevin/maybe-alltheicon "git" :face 'warning :v-adjust -0.05)
                                  " "
                                  branch)))))
  (use-package nyan-mode
    :ensure t
    :config
    (setq nyan-animate-nyancat nil)
    (nyan-mode t)
    (spaceline-toggle-nyan-cat-on))
  ;; hide the current position in the buffer as a percentage
  (spaceline-toggle-buffer-position-off)
  ;; shows the currently visible part of the buffer.
  (spaceline-toggle-hud-on)
  (setq powerline-height 23)
  (spaceline-emacs-theme))

(provide 'init-modeline)
;;; init-modeline ends here
