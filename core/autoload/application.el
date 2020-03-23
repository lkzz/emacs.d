;;; application.el --- autoload functions used by application -*- lexical-binding: t -*-
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

;;;###autoload
(defun kevin/open-iterm ()
  "Open iTerm and focus on it."
  (interactive)
  (do-applescript
   "do shell script \"open -a iTerm\"\n"))

;;;###autoload
(defun kevin/open-wechat ()
  "Open WeChat and focus on it."
  (interactive)
  (do-applescript
   "do shell script \"open -a WeChat\"\n"))

;;;###autoload
(defun kevin/open-youdao ()
  "Open youdao dictionary and focus on it."
  (interactive)
  (do-applescript
   "do shell script \"open -a 有道词典\"\n"))

;;;###autoload
(defun kevin/open-chrome ()
  "Open chrome dictionary and focus on it."
  (interactive)
  (shell-command "open /Applications/Google\sChrome.app --args --enable-net-benchmarking"))
