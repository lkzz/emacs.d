;;; init-modeline.el --- modeline config for emacs.
;;; Commentary:
;;; Code:

(defmacro modeline-define-segment (name body)
  "Create function NAME by wrapping BODY."
  `(defun ,name
       (&optional face)
     (modeline-render-str ,body face)))

(defun modeline-render-segment (item)
  "Render a modeline segment ITEM."
  (cond
   ((and (listp item) (eq 'image (car item)))
    (propertize " " 'display item
                'face (plist-get (cdr item) :face)))
   (t item)))

(defun modeline-render-segment-list (items)
  "Render a list of powerline VALUES."
  (mapconcat 'modeline-render-segment items ""))

(defun modeline-render-str (str &optional face)
  "Render STR as mode-line data using FACE."
  (when str
    (let ((fstr (format-mode-line str)))
      (if face
          (propertize fstr 'face face)
        fstr))))

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

(defun modeline-fill (reserve &optional face)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face (if face face 'mode-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Custom faces;;;;;;;;;;;;;;;;;;;;;;;
(defface doom-modeline-buffer-path
  '((t (:inherit mode-line-emphasis :bold t)))
  "Face used for the dirname part of the buffer path.")

(defface doom-modeline-buffer-file
  '((t (:inherit mode-line-buffer-id)))
  "Face used for the filename part of the mode-line buffer path.")

(defface doom-modeline-buffer-modified
  '((t (:inherit error :background nil :bold t)))
  "Face used for the 'unsaved' symbol in the mode-line.")

(defface doom-modeline-buffer-major-mode
  '((t (:inherit mode-line-emphasis :bold t)))
  "Face used for the major-mode segment in the mode-line.")

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line.")

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `doom-modeline--anzu', `doom-modeline--evil-substitute' and
`iedit'")(eval-and-compile
  (defvar doom-modeline-fn-alist ())
  (defvar doom-modeline-var-alist ()))

(defface doom-modeline-info
  `((t (:inherit success :bold t)))
  "Face for info-level messages in the modeline. Used by `*vc'.")

(defface doom-modeline-warning
  `((t (:inherit warning :bold t)))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defface doom-modeline-urgent
  `((t (:inherit error :bold t)))
  "Face for errors in the modeline. Used by `*flycheck'")

;; Bar
(defface doom-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window.")

(defface doom-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active.")

(defface doom-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window.")

(defface doom-modeline-persp '((t ()))
  "The face used for persp number.")

(defface doom-modeline-eyebrowse '((t ()))
  "The face used for eyebrowse.")

(defface doom-modeline-bracket '((t (:inherit shadow)))
  "The face used for brackets around the project.")

(defun kevin/maybe-octicon-icon (&rest args)
  "Display octicon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-octicon args)))

(defun kevin/maybe-faicon-icon (&rest args)
  "Display font awesome icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-faicon args)))

(defun kevin/maybe-material-icon (&rest args)
  "Display material icon via `ARGS'."
  (when (display-graphic-p)
    (apply 'all-the-icons-material args)))

(defun kevin/material-icon-with-text (icon &optional text face voffset)
  "Displays an material ICON with FACE, followed by TEXT."
  (concat
   (when icon
     (concat
      (kevin/maybe-material-icon icon :face face :height 1.1 :v-adjust (or voffset -0.2))
      " "
      (if text (propertize text 'face face))))))

(defun kevin/faicon-icon-with-text (icon &optional text face voffset)
  "Displays an faicon ICON with FACE, followed by TEXT."
  (concat
   (when icon
     (concat
      (kevin/maybe-faicon-icon icon :face face :height 1.1 :v-adjust (or voffset -0.2))
      " "
      (if text (propertize text 'face face))))))



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

(modeline-define-segment buffer-info
                         (concat (cond (buffer-read-only
                                        (concat (kevin/maybe-material-icon
                                                 "lock"
                                                 :face 'mode-line-buffer-id
                                                 :v-adjust -0.05)
                                                " "))
                                       ((buffer-modified-p)
                                        (concat (kevin/maybe-faicon-icon
                                                 "floppy-o"
                                                 :face 'doom-modeline-buffer-modified
                                                 :v-adjust -0.0575)
                                                " "))
                                       ((and buffer-file-name
                                             (not (file-exists-p buffer-file-name)))
                                        (concat (kevin/maybe-octicon-icon
                                                 "circle-slash"
                                                 :face 'doom-modeline-urgent
                                                 :v-adjust -0.05)
                                                " "))
                                       ((buffer-narrowed-p)
                                        (concat (kevin/maybe-octicon-icon
                                                 "fold"
                                                 :face 'doom-modeline-warning
                                                 :v-adjust -0.05)
                                                " ")))
                                 (if (buffer-file-name)
                                     (concat (shorten-directory default-directory 15) (file-relative-name buffer-file-name) " ")
                                   "%b ")))

(modeline-define-segment timestamp-info
                         (format-time-string "%H:%M "))

(modeline-define-segment position-info
                         (format-mode-line "%l:%c "))

(modeline-define-segment buffer-encoding
                         (concat (pcase (coding-system-eol-type buffer-file-coding-system)
                                   (0 "LF ")
                                   (1 "CRLF ")
                                   (2 "CR "))
                                 (let ((sys (coding-system-plist buffer-file-coding-system)))
                                   (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                                          "UTF-8")
                                         (t (upcase (symbol-name (plist-get sys :name))))))
                                 " "))

(modeline-define-segment vc-info
                         (when (bound-and-true-p vc-mode)
                           (cond ((string-match "Git[:-]" vc-mode)
                                  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
                                    (kevin/faicon-icon-with-text
                                     "code-fork"
                                     (format "%s " branch)
                                     'doom-modeline-urgent
                                     -0.05)
                                    ))
                                 (t (format "%s " vc-mode)))))

(modeline-define-segment flycheck-info
                         (when (bound-and-true-p flycheck-mode)
                           (concat
                            (pcase flycheck-last-status-change
                              (`finished (if flycheck-current-errors
                                             (let-alist (flycheck-count-errors flycheck-current-errors)
                                               (let ((sum (+ (or .error 0) (or .warning 0))))
                                                 (kevin/material-icon-with-text "do_not_disturb_alt"
                                                                                (number-to-string sum)
                                                                                (if .error 'doom-modeline-urgent 'doom-modeline-warning)
                                                                                -0.25)))
                                           (kevin/material-icon-with-text "check" " " 'doom-modeline-info)))
                              (`running     (kevin/material-icon-with-text "access_time" " " 'font-lock-doc-face -0.25))
                              (`no-checker  (kevin/material-icon-with-text "sim_card_alert" " -" 'font-lock-doc-face))
                              (`errored     (kevin/material-icon-with-text "sim_card_alert" " Error" 'doom-modeline-urgent))
                              (`interrupted (kevin/material-icon-with-text "pause" " Interrupted" 'font-lock-doc-face)))
                            " ")))

(modeline-define-segment minor-modes
                         (format-mode-line minor-mode-alist))

(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((lhs (list (buffer-info)
                                   (position-info 'doom-modeline-warning)
                                   (vc-info)
                                   (minor-modes 'mode-line)
                                   ))
                        (rhs (list
                              (flycheck-info)
                              ;; (buffer-encoding)
                              (timestamp-info 'mode-line)
                              ))
                        )
                   (concat
                    (modeline-render-segment-list lhs)
                    (modeline-fill (modeline-width rhs))
                    (modeline-render-segment-list rhs)
                    )))))


(provide 'init-modeline)
;;; init-modeline ends here
