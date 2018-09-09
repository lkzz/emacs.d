;;; init-etags.el --- Initialize ctags configurations. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(use-package counsel-etags
  :defer t
  :config
  ;; counsel-etags-ignore-directories does NOT support wildcast
  (add-to-list 'counsel-etags-ignore-directories "build_clang")
  ;; counsel-etags-ignore-filenames supports wildcast
  (add-to-list 'counsel-etags-ignore-filenames "TAGS")
  (add-to-list 'counsel-etags-ignore-filenames "*.json")
  (setq tags-file-name "~/Code/gopath/src/go-common/tags"))

(provide 'init-etags)
;;; init-etags.el ends here
