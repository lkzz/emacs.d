;;; init-markdown.el --- custom markdown mode. -*- lexical-binding: t -*-
;;
;;; Author: kevin <kevin.scnu@gmail.com>
;;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :defer t
  :ensure t
  :mode (("\\.mmark\\'" . markdown-mode)
         ("README\\.md$'" . gfm-mode)
         ("\\.md$'" . markdon-mode)
         ("\\.markdown$'" . markdown-mode))
  :init
  (progn
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
      (add-to-list 'auto-mode-alist `(,regex . markdown-mode)))))

(use-package markdown-preview-mode
  :ensure t
  :after markdown-mode
  :config
  (progn
    (setq markdown-command "multimarkdown")
    (setq markdown-preview-auto-open t)))

(provide 'init-markdown)
;;; init-markdown ends here
