;;; core.el -- files must be load in my emacs. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(require 'core-elpa)
(require 'core-variables)
(require 'core-functions)

(byte-recompile-file "~/.emacs.d/core/core.el" nil 0)
(provide 'core)
