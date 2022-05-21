;;; init-buffer.el -- Initialization buffer. -*- lexical-binding: t; -*-
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
;;
;;; Code:

(use-package ibuffer
  :straight (:type built-in)
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

(defun my-auto-save-buffer()
  (let ((autosave-buffer-list))
    (ignore-errors
      (save-excursion
        (dolist (buf (buffer-list))
          (set-buffer buf)
          (when (and
                 ;; evil normal state
                 (evil-normal-state-p)
                 ;; filename is not empty
                 (buffer-file-name)
                 ;; buffer is modified
                 (buffer-modified-p)
                 ;; smerge mode is not active
                 (not (bound-and-true-p smerge-mode))
                 ;; yassnippet is not active
                 (or (not (boundp 'yas--active-snippets))
                     (not yas--active-snippets))
                 ;; company is not active
                 (or (not (boundp 'company-candidates))
                     (not company-candidates))
                 )
            (push (buffer-name) autosave-buffer-list)
            (basic-save-buffer))))
      (cond
       ((= (length autosave-buffer-list) 1)
        (message "# Auto saved %s at %s" (car autosave-buffer-list) (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))
       ((> (length autosave-buffer-list) 1)
        (message "# Auto saved %d files: %s"
                 (length autosave-buffer-list)
                 (mapconcat 'identity autosave-buffer-list "|")))))))

(add-hook 'after-init-hook (lambda ()
                             (run-with-idle-timer 1 t #'my-auto-save-buffer)
                             ;; TODO 测试这里是否可以移除
                             ;; (add-hook 'before-save-hook 'font-lock-flush)
                             ))

(provide 'init-buffer)
;;; init-buffer.el ends here
