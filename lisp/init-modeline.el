;;; init-modeline.el. -*- lexical-binding: t; -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;             modeline config.
;;; Code:

;;;###autoload
(defmacro modeline-define-segment (name body)
  "Create function NAME by wrapping BODY."
  `(defun ,name
       (&optional face)
     (modeline-render-str ,body face)))

;;;###autoload
(defun modeline-render-segment (item)
  "Render a modeline segment ITEM."
  (cond
   ((and (listp item) (eq 'image (car item)))
    (propertize " " 'display item
                'face (plist-get (cdr item) :face)))
   (t item)))

;;;###autoload
(defun modeline-render-segment-list (items)
  "Render a list of powerline VALUES."
  (mapconcat 'modeline-render-segment items ""))

;;;###autoload
(defun modeline-render-str (str &optional face)
  "Render STR as mode-line data using FACE and left space on the right side."
  (when str
    (let ((fstr (format-mode-line str)))
      (if face
          (concat (propertize fstr 'face face) " ")
        (concat fstr " ")))))

;;;###autoload
(defun modeline-width (items)
  "Get the length of ITEMS."
  (if items
      (let ((val (car items)))
        (+ (cond
            ((stringp val) (string-width (format-mode-line val)))
            ((and (listp val) (eq 'image (car val)))
             (car (image-size val)))
            (t 0))
           (modeline-width (cdr items))))
    0))

;;;###autoload
(defun modeline-fill (reserve &optional face)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face (if face face 'mode-line)))

;;;###autoload
(defun kevin/maybe-alltheicon (&rest args)
  "Display octicon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-alltheicon args)))

(defun kevin/maybe-octicon-icon (&rest args)
  "Display octicon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-octicon args)))

;;;###autoload
(defun kevin/maybe-faicon-icon (&rest args)
  "Display font awesome icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-faicon args)))

;;;###autoload
(defun kevin/maybe-material-icon (&rest args)
  "Display material icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-material args)))

;;;###autoload
(defun kevin/material-icon-with-text (icon &optional text face voffset)
  "Displays an material ICON with FACE, followed by TEXT."
  (concat
   (when (display-graphic-p)
     (kevin/maybe-material-icon icon :face face :height 1.1 :v-adjust (or voffset -0.2)))
   " "
   (if text (propertize text 'face face))))

;;;###autoload
(defun kevin/faicon-icon-with-text (icon &optional text face voffset)
  "Displays an faicon ICON with FACE, followed by TEXT."
  (concat
   (when (display-graphic-p)
     (kevin/maybe-faicon-icon icon :face face :height 1.1 :v-adjust (or voffset -0.2)))
   (if text (propertize text 'face face))))

;;;###autoload
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
      (setq output (concat ".../" output)))
    output))

;;;###autoload
(defun modeline-active ()
  "Return whether the current window is active."
  (eq (frame-selected-window) (selected-window)))

(modeline-define-segment buffer-info-segment
                         (concat (cond (buffer-read-only
                                        (kevin/maybe-material-icon "lock"
                                                                   :face 'warning
                                                                   :v-adjust -0.15))
                                       ((buffer-modified-p)
                                        (kevin/maybe-faicon-icon "floppy-o"
                                                                 :face 'error
                                                                 :v-adjust -0.0575))
                                       ((and buffer-file-name
                                             (not (file-exists-p buffer-file-name)))
                                        (kevin/maybe-octicon-icon "circle-slash"
                                                                  :face 'error
                                                                  :v-adjust -0.05))
                                       ((buffer-narrowed-p)
                                        (kevin/maybe-octicon-icon "fold"
                                                                  :face 'warning
                                                                  :v-adjust -0.05))
                                       )
                                 (if (buffer-file-name)
                                     (concat (shorten-directory default-directory 15) (file-relative-name buffer-file-name))
                                   "%b")))



(modeline-define-segment timestamp-info-segment
                         (format-time-string "%H:%M "))

(modeline-define-segment position-info-segment
                         (format-mode-line "%l:%c"))

(modeline-define-segment buffer-encoding-segment
                         (concat (pcase (coding-system-eol-type buffer-file-coding-system)
                                   (0 "LF ")
                                   (1 "CRLF ")
                                   (2 "CR "))
                                 (let ((sys (coding-system-plist buffer-file-coding-system)))
                                   (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                                          "UTF-8")
                                         (t (upcase (symbol-name (plist-get sys :name))))))))

(modeline-define-segment vsc-info-segment
                         (when (bound-and-true-p vc-mode)
                           (cond ((string-match "Git[:-]" vc-mode)
                                  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
                                    (concat
                                     (kevin/maybe-alltheicon "git"
                                                             :face 'warning
                                                             :v-adjust -0.05)
                                     " "
                                     branch)))
                                 (t vc-mode))))

(modeline-define-segment flycheck-segment
                         (when (bound-and-true-p flycheck-mode)
                           (pcase flycheck-last-status-change
                             (`finished (if flycheck-current-errors
                                            (let-alist (flycheck-count-errors flycheck-current-errors)
                                              (let ((sum (+ (or .error 0) (or .warning 0))))
                                                (kevin/material-icon-with-text "error_outline"
                                                                               (number-to-string sum)
                                                                               (if .error 'error 'warning)
                                                                               -0.20)))
                                          (kevin/material-icon-with-text "check" nil 'mode-line)))
                             (`running     (kevin/material-icon-with-text "access_time" nil 'font-lock-doc-face -0.25))
                             (`no-checker  (kevin/material-icon-with-text "sim_card_alert" "-" 'font-lock-doc-face))
                             (`errored     (kevin/material-icon-with-text "do_not_disturb" "Error" 'error -0.15))
                             (`interrupted (kevin/material-icon-with-text "pause" "Interrupted" 'font-lock-doc-face)))
                           ))

;; Return simplifyed major mode name.
(modeline-define-segment major-mode-segment
                         (let* ((major-name (format-mode-line "%m"))
                                (replace-table '(Emacs-Lisp "Elisp"
                                                            Python "Py"
                                                            Shell ">"
                                                            Makrdown "MD"
                                                            GFM "𝓜"
                                                            Org "lrg"
                                                            Text "𝓣ext"
                                                            Fundamental "ℱ"
                                                            ))
                                (replace-name (plist-get replace-table (intern major-name))))
                           (if replace-name replace-name major-name)))

(modeline-define-segment minor-mode-segment
                         (when (and (display-graphic-p) (> (window-width) 120))
                           (format-mode-line minor-mode-alist)))

(modeline-define-segment window-number-segment
                         (when (bound-and-true-p window-numbering-mode)
                           (let* ((num (window-numbering-get-number))
                                  (str (when num (int-to-string num))))
                             (cond
                              ((string= "1" str) "➊")
                              ((string= "2" str) "➋")
                              ((string= "3" str) "➌")
                              ((string= "4" str) "➍")
                              ((string= "5" str) "➎")
                              ((string= "6" str) "➏")
                              ((string= "7" str) "➐")
                              ((string= "8" str) "➑")
                              ((string= "9" str) "➒")
                              ((string= "0" str) "➓"))
                             )))

(with-eval-after-load 'evil
  (setq evil-normal-state-tag (kevin/faicon-icon-with-text "chevron-right" "NO" 'mode-line))
  (setq evil-insert-state-tag (kevin/faicon-icon-with-text "chevron-right" "IN" 'success))
  (setq evil-motion-state-tag (kevin/faicon-icon-with-text "chevron-right" "MO" 'warning))
  (setq evil-visual-state-tag (kevin/faicon-icon-with-text "chevron-right" "VI" 'error))
  (setq evil-operator-state-tag (kevin/faicon-icon-with-text "chevron-right" "OP" 'font-lock-doc-face))
  )
(modeline-define-segment evil-tag-segment
                         (when (bound-and-true-p evil-mode)
                           evil-mode-line-tag
                           ))

(defun modeline-init ()
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (modeline-active))
                          (face0 (if active 'mode-line 'mode-line-inactive))
                          (lhs (list
                                (evil-tag-segment)
                                (window-number-segment 'face0)
                                (buffer-info-segment 'face0)
                                (minor-mode-segment 'face0)
                                (position-info-segment 'face0)
                                ))
                          (rhs (list
                                (vsc-info-segment 'warning)
                                (major-mode-segment)
                                (flycheck-segment)
                                (buffer-encoding-segment 'face0)
                                (timestamp-info-segment 'face0)
                                ))
                          )
                     (concat
                      (modeline-render-segment-list lhs)
                      (modeline-fill (modeline-width rhs) 'face0)
                      (modeline-render-segment-list rhs)
                      )))))

  )

(add-hook 'after-init-hook #'modeline-init)

(provide 'init-modeline)
;;; init-modeline ends here
