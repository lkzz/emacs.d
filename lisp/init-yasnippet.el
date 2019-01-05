;;; init-yasnippet.el --- Initialize yasnippet configurations. -*- lexical-binding: t; -*-
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

(use-package yasnippet
  :defer t
  :ensure t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :defer t
  :ensure t
  :after (yasnippet)
  :ensure t)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
