;;; init-elfeed.el --- setup Emacs web feeds client. -*- lexical-binding: t; -*-
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

(use-package elfeed
  :bind (:map elfeed-search-mode-map
              ("A" . bjm/elfeed-show-all)
              ("E" . bjm/elfeed-show-emacs)
              ("D" . bjm/elfeed-show-daily)
              ("q" . bjm/elfeed-save-db-and-bury)))

;; use an org file to organise feeds
(use-package elfeed-org
  :config
  (setq rmh-elfeed-org-files (list "/path/to/elfeed.org")))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
