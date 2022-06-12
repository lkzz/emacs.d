;;; init-completion.el --- completion config. -*- lexical-binding: t -*-
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
;;; Code:

;; Completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enhance completion at point.
(use-package corfu
  :straight (corfu :includes (corfu-indexed corfu-quick) :files (:defaults "extensions/corfu-*.el"))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-count 15)
  (corfu-bar-width 0.5)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-max-width 100)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-preview-current nil)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("C-n" . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("C-p" . corfu-previous)
              ("C-j" . corfu-insert)
              ("C-g" . corfu-quit))
  :init (global-corfu-mode)
  :config
  (setq corfu-excluded-modes '(shell-mode
                               eshell-mode
                               comint-mode
                               erc-mode
                               gud-mode
                               rcirc-mode
                               text-mode
                               minibuffer-inactive-mode))
  (advice-add #'keyboard-quit :before #'corfu-quit)
  (with-eval-after-load 'evil
    ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
    (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map)
    ;; auto quit corfu when exit insert state
    (add-hook 'evil-normal-state-entry-hook (lambda ()
                                              (when corfu--candidates (corfu-quit)))))

  ;; extensions
  (use-package corfu-quick
    :straight nil
    :after corfu
    :bind (:map corfu-map
                ("C-q" . corfu-quick-insert)))
  (use-package corfu-history
    :straight nil
    :after corfu
    :hook (corfu-mode . corfu-history-mode))

  (use-package corfu-doc
    :after corfu
    :straight (:host github :repo "galeo/corfu-doc")
    :hook (corfu-mode . corfu-doc-mode)
    :bind (:map corfu-map
                ("K" . corfu-doc-toggle)))

  ;; A bunch of completion at point extensions
  (use-package cape
    :after corfu
    :config
    ;; 默认补全后端
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

  (use-package tabnine-capf
    :after cape
    :straight (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh"))
    :hook (kill-emacs . tabnine-capf-kill-process)
    :config
    (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point))

  (defun my/set-mixed-capf ()
    (setq-local completion-category-defaults nil)
    (setq-local completion-at-point-functions (list
		                                       (cape-capf-buster
                                                (cape-super-capf
                                                 (pcase my-lsp-backend
                                                   ('lsp-bridge #'lsp-bridge-capf)
                                                   ('eglot #'eglot-completion-at-point)
                                                   ('lsp-mode #'lsp-completion-at-point)
                                                   (_ #'eglot-completion-at-point))
                                                 #'tabnine-completion-at-point
                                                 #'cape-file
                                                 #'cape-keyword
                                                 #'cape-dabbrev)
                                                'equal))))

  ;; (add-hook 'lsp-bridge-mode-hook #'my/set-mixed-capf)
  (add-hook 'eglot-managed-mode-hook #'my/set-mixed-capf)
  (add-hook 'lsp-completion-mode-hook #'my/set-mixed-capf))

(provide 'init-completion)
;;; init-completion.el ends here
