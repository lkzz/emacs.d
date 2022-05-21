;;; network.el --- insert description here -*- lexical-binding: t -*-
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

;;;###autoload
(defun my-show-http-proxy ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" my-http-proxy)
    (message "No HTTP proxy")))

;;;###autoload
(defun my-enable-http-proxy ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,my-http-proxy)
          ("https" . ,my-http-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (my-show-http-proxy))

;;;###autoload
(defun my-disable-http-proxy ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (my-show-http-proxy))

;;;###autoload
(defun my-toggle-http-proxy ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (my-disable-http-proxy)
    (my-enable-http-proxy)))

;;; network.el ends here
