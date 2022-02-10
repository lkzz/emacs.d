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
(defun kevin/show-http-proxy ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" kevin-http-proxy)
    (message "No HTTP proxy")))

;;;###autoload
(defun kevin/enable-http-proxy ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,kevin-http-proxy)
          ("https" . ,kevin-http-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (kevin/show-http-proxy))

;;;###autoload
(defun kevin/disable-http-proxy ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (kevin/show-http-proxy))

;;;###autoload
(defun kevin/toggle-http-proxy ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (kevin/disable-http-proxy)
    (kevin/enable-http-proxy)))

;;; network.el ends here
