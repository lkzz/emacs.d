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
  :demand t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch)))

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
    :after corfu
    :bind (:map corfu-map
                ("C-q" . corfu-quick-insert)))

  (use-package corfu-doc
    :after corfu
    :straight (:host github :repo "galeo/corfu-doc")
    :hook (corfu-mode . corfu-doc-mode)
    :bind (:map corfu-map
                ("K" . corfu-doc-toggle)))

  ;; A bunch of completion at point extensions
  (use-package cape
    :after corfu
    :hook ((lsp-completion-mode eglot-managed-mode lsp-bridge-mode). my/set-mixed-capf)
    :config
    (defun my/set-mixed-capf ()
      (setq-local completion-category-defaults nil)
      (setq-local completion-at-point-functions (list
		                                         (cape-capf-buster
                                                  (cape-super-capf
                                                   #'lsp-bridge-capf
                                                   ;; #'eglot-completion-at-point
                                                   ;; #'lsp-completion-at-point
                                                   #'cape-file
                                                   #'cape-keyword
                                                   #'cape-dabbrev
                                                   )
                                                  'equal)
		                                         )))
    ;; 默认补全后端
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-file)))

(provide 'init-completion)
;;; init-completion.el ends here
