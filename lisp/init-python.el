;;; init-python.el --- Initialize python configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;   pip install pyflakes
;;   pip install autopep8
;;
;;; Code:

;; Python Mode
(use-package python
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . lsp-deferred)
  :init
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  ;; Pyright: https://github.com/emacs-lsp/lsp-pyright
  (use-package lsp-pyright
    :hook (python-mode . (lambda () (require 'lsp-pyright)))
    :init (when (executable-find "python3")
            (setq lsp-pyright-python-executable-cmd "python3")))

  ;; Format buffer
  (use-package yapfify
    :config
    (add-hook 'python-mode-hook 'yapf-mode)))

(provide 'init-python)
;;; init-python.el ends here
