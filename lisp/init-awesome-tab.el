;; init-awesome-tab.el -- initialize tab configurations. -*- lexical-binding: t; -*-
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

(use-package awesome-tab
  :disabled
  :load-path "vendor/awesome-tab"
  (kevin/comma-leader-add
    "tt" 'awesome-tab-switch-group
    "ta" 'awesome-tab-select-beg-tab
    "te" 'awesome-tab-select-end-tab
    "t<" 'awesome-tab-move-current-tab-to-left
    "t>" 'awesome-tab-move-current-tab-to-right
    "th" 'awesome-tab-forward
    "tl" 'awesome-tab-backward)
  :config
  (setq awesome-tab-cycle-scope 'tabs) ; Navigate through visible tabs only.
  (awesome-tab-mode t))

(provide 'init-awesome-tab)
