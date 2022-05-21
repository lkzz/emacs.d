;;; init-format.el --- code format config. -*- lexical-binding: t -*-
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


(use-package format-all
  :diminish
  :commands format-all-buffer
  :hook ((prog-mode) . format-all-ensure-formatter))

(provide 'init-format)
;;; init-format.el ends here
