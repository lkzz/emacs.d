;;; init-osx.el --- special config for mac osx. -*- lexical-binding: t; -*-
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
(use-package exec-path-from-shell
  :ensure t
  :defer t
  :if kevin-mac-p
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH" "GOROOT"))
  (setq exec-path-from-shell-arguments '("-l"))
  (add-hook 'after-init-hook 'exec-path-from-shell-initialize))

(use-package counsel-osx-app
  :ensure t
  :defer t
  :if kevin-mac-p
  :init
  (kevin/set-leader-keys "aa" 'counsel-osx-app))

(use-package reveal-in-osx-finder
  :ensure t
  :defer t
  :if kevin-mac-p
  :commands reveal-in-osx-finder
  :init
  (kevin/set-leader-keys "br" 'reveal-in-osx-finder))

;; Use the OS X Emoji font for Emoticons
(when (and kevin-mac-p (fboundp 'set-fontset-font))
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend)

  )

(provide 'init-osx)
;;; init-osx ends here
