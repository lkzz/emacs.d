;;; init-python.el --- Initialize python configurations. -*- lexical-binding: t; -*-
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
;;   pip install pyflakes
;;   pip install autopep8
;;
;;; Code:

;; Python Mode
(use-package python
  :ensure nil
  :custom
  (python-indent-offset 4)
  (python-indent-guess-indent-offset nil)
  :config
  (setq python-shell-completion-native-enable nil))

;; Anaconda mode
(use-package anaconda-mode
  :after python
  :diminish anaconda-mode
  :hook (python-mode . anaconda-mode)
  :init
  (kevin/set-leader-keys-for-major-mode 'python-mode
                                        "hh" 'anaconda-mode-show-doc
                                        "ga" 'anaconda-mode-find-assignments
                                        "gb" 'xref-pop-marker-stack
                                        "gu" 'anaconda-mode-find-references)
  (setq anaconda-mode-installation-directory
        (concat kevin-cache-directory "anaconda-mode")))

(use-package company-anaconda
  :after (company anaconda-mode)
  :preface
  (defun kevin/setup-python-company-backends ()
    (make-local-variable 'company-backends)
    (setq company-backends (list 'company-anaconda 'company-dabbrev 'company-keywords 'company-yasnippet)))
  :hook (python-mode . #'kevin/setup-go-company-backends))

;; Autopep8
(use-package py-autopep8
  :after python
  :hook (python-mode . py-autopep8-enable-on-save))

(provide 'init-python)
;;; init-python.el ends here
