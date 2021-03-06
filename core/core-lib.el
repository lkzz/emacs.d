;;; core-lib.el -- 一些常用的宏和函数. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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

(require 'autoload)

(defun kevin/generate-autoload-define (loaddef &rest DIRS)
  (let ((generated-autoload-file loaddef))
    (when (not (file-exists-p loaddef))
      (with-current-buffer (find-file-noselect generated-autoload-file)
	    (insert ";; generated by function: `kevin/generate-autoload-define'.")
	    (save-buffer)))
    (apply 'update-directory-autoloads DIRS)))

(defun kevin/reload-core-autoloads ()
  "Reload autoload function from `core/autoload' and save in `kevin-autoload-file'."
  (interactive)
  (when (file-exists-p kevin-autoload-file)
    (delete-file kevin-autoload-file t)
    (message "delete old autoload file: %s" kevin-autoload-file))

  (kevin/generate-autoload-define kevin-autoload-file (concat user-emacs-directory "core/autoload/"))
  (load kevin-autoload-file nil 'nomessage)
  (message "reload core autoload file: %s done." kevin-autoload-file))

(when (not (file-exists-p kevin-autoload-file))
  (kevin/generate-autoload-define kevin-autoload-file (concat user-emacs-directory "core/autoload/"))
  (byte-compile-file kevin-autoload-file)
  (message "generate autoload file: %s done." kevin-autoload-file))
(load kevin-autoload-file nil 'nomessage)

(provide 'core-lib)
;;; core-lib.el ends here
