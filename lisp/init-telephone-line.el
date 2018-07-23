;;; init-telephone-line.el --- modeline config for emacs.
;;; Commentary:
;;; Code:

(use-package telephone-line
  :init
  (progn
    (setq telephone-line-height 30)
    (setq telephone-line-primary-left-separator 'telephone-line-abs-left)
    (setq telephone-line-primary-right-separator 'telephone-line-abs-right)
    (setq telephone-line-secondary-left-separator 'telephone-line-abs-left)
    (setq telephone-line-secondary-right-separator 'telephone-line-abs-right)
    (telephone-line-defsegment* kevin/vc-info ()
      (when (bound-and-true-p vc-mode)
        (cond ((string-match "Git[:-]" vc-mode)
               (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
                 (concat
                  (propertize (format "%s" (all-the-icons-faicon "code-fork" :v-adjust -0.1))
                              'face `(:foreground "orange" :height 1.0))
                  (propertize (format " %s" branch) 'face `(:foreground "yellow" :height 1.0)))))
              (t (format "%s" vc-mode)))))

    (telephone-line-defsegment* kevin/flycheck-segment ()
      (when (bound-and-true-p flycheck-mode)
        (let* ((text (pcase flycheck-last-status-change
                       ('finished (if flycheck-current-errors
                                      (let-alist (flycheck-count-errors flycheck-current-errors)
                                        (if (or .error .warning)
                                            (concat
                                             (propertize (format "%s" (all-the-icons-faicon "ban" :v-adjust -0.1))
                                                         'face `(:foreground "orange" :height 1.0))
                                             (propertize (format " %s/%s"
                                                                 (or .error 0) (or .warning 0))
                                                         'face '(:foreground "yellow" :height 1.0)))
                                          ""))
                                    ":)"))
                       ('running     "*")
                       ('no-checker  "")
                       ('not-checked "=")
                       ('errored     (propertize "!" 'face '(:foreground "tomato")))
                       ('interrupted (propertize "." 'face '(:foreground "tomato")))
                       ('suspicious  "?"))))
          (propertize text
                      'help-echo (pcase flycheck-last-status-change
                                   ('finished "Display errors found by Flycheck")
                                   ('running "Running...")
                                   ('no-checker "No Checker")
                                   ('not-checked "Not Checked")
                                   ('errored "Error!")
                                   ('interrupted "Interrupted")
                                   ('suspicious "Suspicious?"))
                      'display '(raise 0.0)
                      'mouse-face '(:box 1)
                      'local-map (make-mode-line-mouse-map
                                  'mouse-1 #'flycheck-list-errors)))))
    (defun shorten-directory (dir max-length)
      "Setup a directory(`DIR') `MAX-LENGTH' characters."
      (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
            (output ""))
        (when (and path (equal "" (car path)))
          (setq path (cdr path)))
        (while (and path (< (length output) (- max-length 4)))
          (setq output (concat (car path) "/" output))
          (setq path (cdr path)))
        (when path
          (setq output (concat "../" output)))
        output))

    (telephone-line-defsegment* kevin/buffer-name-segment ()
      (concat
       (propertize (format "%s" (all-the-icons-faicon "file-text" :v-adjust -0.1))
                   'face `(:foreground "orange" :height 1.0))
       (propertize (format " %s" (concat (shorten-directory default-directory 15) (file-relative-name buffer-file-name) )))
       ))
    )
  :config
  (progn
    (setq telephone-line-lhs
          '((evil   . (telephone-line-evil-tag-segment))
            (nil    . (kevin/buffer-name-segment
                       telephone-line-minor-mode-segment
                       ))))
    (setq telephone-line-rhs
          '((nil    . (telephone-line-misc-info-segment
                       kevin/vc-info
                       kevin/flycheck-segment))
            (evil   . (telephone-line-airline-position-segment))))
    (telephone-line-mode 1)))

(provide 'init-telephone-line)
;;; init-telephone.el ends here
