;;; init-osx.el --- special config for mac osx. -*- lexical-binding: t; -*-
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
(use-package exec-path-from-shell
  :if is-mac-p
  :hook (after-init . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH")
        exec-path-from-shell-arguments '("-l")))

(use-package counsel-osx-app
  :if is-mac-p
  :commands counsel-osx-app)

(provide 'init-osx)
;;; init-osx ends here
