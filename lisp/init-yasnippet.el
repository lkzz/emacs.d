;;; init-yasnippet.el --- insert description here -*- lexical-binding: t -*-
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
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :bind (("M-/" . company-yasnippet)
         :map company-active-map
         ("C-/" . yas-expand-from-trigger-key))
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets"))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
