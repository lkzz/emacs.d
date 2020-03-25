;;; init-cmake.el --- insert description here -*- lexical-binding: t -*-
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

(use-package cmake-mode
  :mode (("CMakeLists\\.txt$" . cmake-mode)
         ("\\.cmake$'" . cmake-mode))
  :config
  (setq cmake-tab-width 4)
  (add-to-list 'company-backends 'company-cmake)

  (use-package cmake-font-lock
    :config
    (add-hook 'cmake-mode-hook 'font-lock-refresh-defaults)))

(provide 'init-cmake)
;;; init-cmake.el ends here
