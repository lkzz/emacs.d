;;; init-modeline.el --- init spaceline. -*- lexical-binding: t; -*-
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

;; fix icon background color
;; https://github.com/domtronn/all-the-icons.el/issues/131
(defun kevin/propertize-icon (icon)
  (add-face-text-property
   0 (length icon)
   :inherit t icon)
  icon)

;;;###autload
(defun kevin/maybe-alltheicon (&rest args)
  "Display octicon via `ARGS'."
  (when (display-graphic-p)
    (kevin/propertize-icon (apply 'all-the-icons-alltheicon args))))

;;;###autload
(defun kevin/maybe-faicon-icon (&rest args)
  "Display font awesome icon via `ARGS'."
  (when (display-graphic-p)
    (kevin/propertize-icon (apply 'all-the-icons-faicon args))))

;;;###
(defun shorten-directory (dir max-length)
  "Setup a directory(`DIR') `MAX-LENGTH' characters."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(use-package nyan-mode
  :if (display-graphic-p)
  :init
  (setq nyan-animate-nyancat nil)
  (nyan-mode t))

(use-package anzu
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config
  (setq anzu-replace-to-string-separator (if (char-displayable-p ?→) " → " " -> ")
        ;; let spaceline handle auzu info in modeline
        anzu-cons-mode-line-p nil))

(use-package powerline
  :defer t
  :config
  (setq powerline-height 23
        powerline-default-separator (if (display-graphic-p) 'arrow 'utf-8)))

(use-package spaceline
  :defer t
  :init
  (add-hook 'after-init-hook (lambda () (require 'spaceline)))
  :config
  ;; To get the mode-line highlight to change color depending on the evil state
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

  (use-package spaceline-segments
    :ensure nil
    :config
    (setq spaceline-window-numbers-unicode t
          spaceline-minor-modes-separator ""
          spaceline-workspace-numbers-unicode t)
    ;;define version control segment
    (spaceline-define-segment version-control
      "Version control information."
      (when vc-mode
        (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
          (powerline-raw (concat (kevin/maybe-alltheicon "git" :face 'warning :v-adjust -0.05)
                                 " "
                                 branch)))))
    ;;define major mode segment
    (spaceline-define-segment major-mode
      "Return simplifyed major mode name."
      (let* ((major-name (format-mode-line "%m"))
             (replace-table '(
                              Emacs-Lisp "Elisp"
                              Shell-script ""
                              Python ""
                              Go ""
                              C++//l "C++"
                              Protocol-Buffers//l "PB"
                              Fundamental ""))
             (replace-name (plist-get replace-table (intern major-name))))
        (if replace-name
            replace-name major-name)))
    ;; define buffer id segment
    (spaceline-define-segment buffer-id
      "Shorten buufer fileanme."
      (when (buffer-file-name)
        (concat
         (kevin/maybe-faicon-icon "floppy-o" :face 'warning :v-adjust -0.05)
         " "
         (shorten-directory default-directory 6)
         (file-relative-name buffer-file-name)))))

  (use-package spaceline-config
    :ensure nil
    :config
    (spaceline-toggle-persp-name-on)
    (spaceline-toggle-workspace-number-on)
    (spaceline-toggle-window-number-on)
    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-remote-host-off)
    (spaceline-toggle-flycheck-info-off)
    (spaceline-toggle-selection-info-on)
    (spaceline-toggle-input-method-on)
    (spaceline-toggle-buffer-encoding-abbrev-on)
    (spaceline-toggle-nyan-cat-on)
    ;; hide the current position in the buffer as a percentage
    (spaceline-toggle-buffer-position-off)
    ;; shows the currently visible part of the buffer.
    (spaceline-toggle-hud-off)
    (spaceline-toggle-major-mode-on)
    ;; configure the separator between the minor modes
    (unless (display-graphic-p)
      (spaceline-toggle-minor-modes-off))
    ;; custom spaceline theme
    (spaceline-compile
      ;; define spaceline theme name: spaceline-ml-custom
      "custom"
      ;; left side
      '(((((persp-name :fallback workspace-number) window-number) :separator "")
         :fallback evil-state
         :face highlight-face
         :priority 100)
        (anzu :priority 95)
        ((buffer-id) :priority 98)
        (process :when active)
        ((flycheck-error flycheck-warning flycheck-info) :when active :priority 99)
        (version-control :when active :priority 97)
        (org-pomodoro :when active)
        (org-clock :when active)
        (nyan-cat :when active :priority 70)
        (major-mode :when active :priority 79)
        (minor-modes :when active :priority 78))
      ;; right side
      '((purpose :priority 94)
        (selection-info :priority 95)
        input-method
        ((buffer-encoding-abbrev line-column) :separator "|" :priority 96)
        (global :when active)
        (hud :priority 99)))

    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-custom))))))

(provide 'init-modeline)
;;; init-modeline ends here
