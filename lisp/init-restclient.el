;;; init-restclient.el ---  setup http restclient. -*- lexical-binding: t; -*-
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

(use-package restclient
  :defer t
  :init
  (progn
    ;; (unless restclient-use-org
    (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
    ;; )
    (kevin/set-leader-keys-for-major-mode 'restclient-mode
      "n" 'restclient-jump-next
      "p" 'restclient-jump-prev
      "s" 'restclient-http-send-current-stay-in-window
      "S" 'restclient-http-send-current
      "r" 'restclient-http-send-current-raw
      "y" 'restclient-copy-curl-command)))

(defun kevin/setup-restclient-backends ()
  (let ((local-restclient-backends kevin/company-global-backends))
    (add-to-list 'local-restclient-backends 'company-restclient)
    (set (make-local-variable 'company-backends) local-restclient-backends)))

(use-package company-restclient
  :hook (restclient-mode . kevin/setup-restclient-backends))

(provide 'init-restclient)
;;; init-restclient ends here.
