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
  :general
  (general-define-key
   :states 'normal
   (kbd ",tt") 'awesome-tab-switch-group
   (kbd ",ta") 'awesome-tab-select-beg-tab
   (kbd ",te") 'awesome-tab-select-end-tab
   (kbd ",t<") 'awesome-tab-move-current-tab-to-left
   (kbd ",t>") 'awesome-tab-move-current-tab-to-right
   (kbd ",th") 'awesome-tab-forward
   (kbd ",tl") 'awesome-tab-backward)
  :config
  (setq awesome-tab-cycle-scope 'tabs) ; Navigate through visible tabs only.
  (awesome-tab-mode t))

(provide 'init-awesome-tab)
