;;; init-yasnippet.el --- insert description here -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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
  :hook (prog-mode . yas-minor-mode)
  :config
  (use-package yasnippet-snippets))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
