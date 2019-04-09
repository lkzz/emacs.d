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
  :mode "\\.py\\'"
  :init
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil)
  :config
  (setq python-shell-completion-native-enable nil
        py-python-command "python3"
        python-shell-interpreter "python3"))

;; Anaconda mode
(use-package anaconda-mode
  :after python
  :diminish anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :init
  (kevin/set-leader-keys-for-major-mode 'python-mode
                                        "hh" 'anaconda-mode-show-doc
                                        "ga" 'anaconda-mode-find-assignments
                                        "gb" 'xref-pop-marker-stack
                                        "gu" 'anaconda-mode-find-references)
  (setq anaconda-mode-installation-directory (concat kevin-cache-directory "anaconda-mode")))

(use-package company-anaconda
  :after (company python)
  :init
  (cl-pushnew 'company-anaconda company-backends))

;; Autopep8
(use-package py-autopep8
  :after python
  :hook (python-mode . py-autopep8-enable-on-save))

(provide 'init-python)
;;; init-python.el ends here
