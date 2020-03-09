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
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY" "GOBIN" "GOSUMDB" "RIME_PATH")))

(use-package counsel-osx-app
  :if is-mac-p
  :commands counsel-osx-app
  :general
  (kevin/space-key-define
    "o" '(nil :which-key "Open")
    "o a" 'counsel-osx-app
    "o t" '(kevin/open-iterm :wk "open-item2")
    "o w" '(kevin/open-wechat :wk "open-wechat")
    "o y" '(kevin/open-youdao :wk "open-youdao")))

;; Use the OS X Emoji font for Emoticons
(when (and is-mac-p (fboundp 'set-fontset-font))
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend))

(provide 'init-osx)
;;; init-osx ends here
