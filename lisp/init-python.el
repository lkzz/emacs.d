;;; init-python.el --- Initialize python configurations. -*- lexical-binding: t; -*-
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
;;   pip install pyflakes
;;   pip install autopep8
;;
;;; Code:

;; Python Mode
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil)
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  (setq python-shell-completion-native-enable nil
        py-python-command "python3"
        python-shell-interpreter "python3")

  ;; Anaconda mode
  (use-package anaconda-mode
    :diminish anaconda-mode
    :hook ((python-mode . anaconda-mode)
           (python-mode . anaconda-eldoc-mode))
    :config
    (setq anaconda-mode-installation-directory (concat kevin-cache-dir "anaconda-mode")))

  (use-package company-anaconda
    :init
    (cl-pushnew 'company-anaconda company-backends))

  ;; Autopep8
  (use-package py-autopep8
    :hook (python-mode . py-autopep8-enable-on-save))
  )

(provide 'init-python)
;;; init-python.el ends here
