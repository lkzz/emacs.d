;;; init-markdown.el --- custom markdown mode. -*- lexical-binding: t; -*-
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

(use-package markdown-mode
  :ensure t
  :mode (("\\.mmark\\'" . markdown-mode)
         ("README\\.md$'" . gfm-mode)
         ("\\.md$'" . markdon-mode)
         ("\\.markdown$'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  ;; Redefine the `auto-mode-alist' entries provided by
  ;; `markdown-mode', because `markdown-mode' adds them to the end of
  ;; the list, and in Emacs 26 an earlier entry takes precedence to
  ;; cause files named "CHANGELOG.md" to open in ChangeLog mode
  ;; instead of Markdown mode.
  ;; issue https://github.com/jrblevin/markdown-mode/issues/331
  (dolist (regex '("\\.md\\'" "\\.markdown\\'"))
    (setq auto-mode-alist
          (cl-remove regex auto-mode-alist :test #'equal :key #'car))
    (add-to-list 'auto-mode-alist `(,regex . markdown-mode))))

(use-package markdown-preview-mode
  :defer t
  :ensure t
  :after markdown-mode
  :config
  (setq markdown-command "multimarkdown")
  (setq markdown-preview-auto-open t))

(provide 'init-markdown)
;;; init-markdown ends here
