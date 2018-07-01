;;; init-modeline.el --- modeline config for emacs.
;;; Commentary:
;;; Code:

(use-package all-the-icons)

(defun kevin/add-faicon-icon (icon &optional face help-echo)
  "Retorna una cadena de texto formateada con `propertize' de un icono de all-the-icons"
  (propertize (all-the-icons-faicon icon)
              'face face
              'display '(raise -0.1)
              'help-echo help-echo))


(defun pl/render (item)
  "Render a powerline ITEM."
  (cond
   ((and (listp item) (eq 'image (car item)))
    (propertize " " 'display item
                'face (plist-get (cdr item) :face)))
   (item item)))

(defun pl/property-substrings (str prop)
  "Return a list of substrings of STR when PROP change."
  (let ((beg 0) (end 0)
        (len (length str))
        (out))
    (while (< end (length str))
      (setq end (or (next-single-property-change beg prop str) len))
      (setq out (append out (list (substring str beg (setq beg end))))))
    out))

(defun pl/assure-list (item)
  "Assure that ITEM is a list."
  (if (listp item)
      item
    (list item)))

(defun pl/add-text-property (str prop val)
  (mapconcat
   (lambda (mm)
     (let ((cur (pl/assure-list (get-text-property 0 'face mm))))
       (propertize mm 'face (append cur (list val)))))
   (pl/property-substrings str prop)
   ""))

(defun powerline-render (values)
  "Render a list of powerline VALUES."
  (mapconcat 'pl/render values ""))

(defun powerline-width (values)
  "Get the length of VALUES."
  (if values
      (let ((val (car values)))
        (+ (cond
            ((stringp val) (string-width (format-mode-line val)))
            ((and (listp val) (eq 'image (car val)))
             (car (image-size val)))
            (t 0))
           (powerline-width (cdr values))))
    0))

;;;###autoload
(defun kevin/selected-window-active ()
  "Return whether the current window is active."
  (eq (frame-selected-window) (selected-window)))



;;;###autoload
(defun powerline-raw (str &optional face pad)
  "Render STR as mode-line data using FACE and optionally PAD import on left (l) or right (r)."
  (when str
    (let* ((rendered-str (format-mode-line str))
           (padded-str (concat
                        (when (and (> (length rendered-str) 0) (eq pad 'l)) " ")
                        (if (listp str) rendered-str str)
                        (when (and (> (length rendered-str) 0) (eq pad 'r)) " "))))

      (if face
          (pl/add-text-property padded-str 'face face)
        padded-str))))

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

(defun kevin/directory-mode-line (&optional face pad)
  "Setup directory display in modeline."
  (if (buffer-file-name)
      (let ((filename (concat (shorten-directory default-directory 15)
                              (file-relative-name buffer-file-name))))
        (powerline-raw filename face pad)
        ;; (kevin/insert-faicon-icon-with-text "file-text-o" filename "gray")
        )))

;; 为了对称,我们在buffer-id后也加一个空格:
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b "))


;;;###autoload (autoload 'powerline-buffer-id "powerline")
(defun powerline-buffer-id (&optional face pad)
  (powerline-raw
   (format-mode-line
    (concat " " (propertize
		         (format-mode-line mode-line-buffer-identification)
		         'face face
		         'mouse-face 'mode-line-highlight
		         'help-echo "Buffer name\n\ mouse-1: Previous buffer\n\ mouse-3: Next buffer"
		         'local-map (let ((map (make-sparse-keymap)))
			                  (define-key map [mode-line mouse-1] 'mode-line-previous-buffer)
			                  (define-key map [mode-line mouse-3] 'mode-line-next-buffer)
			                  map))))
   face pad))

(defun kevin/persp-mode-line ()
  "Display layout."
  '(:eval
    (when (bound-and-true-p persp-mode)
      (progn
        (or (not (string= persp-nil-name (safe-persp-name (get-frame-persp))))
            "Default")
        (let ((name (safe-persp-name (get-frame-persp))))
          (kevin/insert-face-text  (concat "[" name "]")  'font-lock-preprocessor-face)
          (propertize (concat "[" name "] ")
                      'face 'font-lock-preprocessor-face
                      'help-echo "Current Layout name."))))))

;; 简化 major-mode 的名字，替换表中没有的显示原名
(defun kevin/simplify-major-mode-name ()
  "Return simplifyed major mode name."
  '(:eval
    (let* ((major-name (format-mode-line "%m"))
           (replace-table '(Emacs-Lisp "Elisp"
                                       Spacemacs\ buffer "𝓢"
                                       Python "Py"
                                       Shell ">"
                                       Makrdown "MD"
                                       GFM "𝓜"
                                       Org "lrg"
                                       Text "𝓣ext"
                                       Fundamental "ℱ"
                                       ))
           (replace-name (plist-get replace-table (intern major-name))))
      (if replace-name replace-name major-name))))

(defun kevin/buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  '(:eval
    (let ((buf-coding (format "%s" buffer-file-coding-system)))
      (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
          (match-string 1 buf-coding)
        buf-coding))))


(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))


(defun kevin/window-number-mode-line ()
  "The current window number,requires `window-numbering-mode' to be enabled."
  (when (bound-and-true-p window-numbering-mode)
    '(:eval
      (let* ((num (window-numbering-get-number))
             (str (when num (int-to-string num))))
        (spaceline--unicode-number (kevin/insert-face-text str 'font-lock-type-face))))))

(defun spaceline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
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
   ((string= "0" str) "➓")))

(defun kevin/buffer-state-info ()
  "Display buffer state info."
  '(:eval
    (let ((overwrite "Ovr") (buffer-modified ""))
      (if overwrite-mode
          (setq overwrite "Ovr")
        (setq overwrite "Ins"))
      (when (buffer-modified-p)
        (setq buffer-modified ",Mod"))
      (when (buffer-modified-p)
        (setq buffer-modified ",RO"))
      (kevin/insert-face-text  (format "[%s%s]" overwrite buffer-modified) "red"))))

(defun kevin/vc-mode-line (&optional face pad)
  (when (bound-and-true-p vc-mode)
    (cond ((string-match "Git[:-]" vc-mode)
           (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
             (powerline-raw branch face pad)
             ;; (kevin/insert-faicon-icon-with-text "code-fork" branch "yellow")
             ))
          (t (format "%s" vc-mode)))))

(defun kevin/flycheck-mode-line ()
  (when (bound-and-true-p flycheck-mode)
    (let* ((text (pcase flycheck-last-status-change
                   ('finished (if flycheck-current-errors
                                  (let-alist (flycheck-count-errors flycheck-current-errors)
                                    (if (or .error .warning)
                                        (kevin/insert-faicon-icon-with-text "ban"
                                                                            (format "%s/%s " (or .error 0) (or .warning 0))
                                                                            "red")
                                      ""))))
                   ('running     "*")
                   ('no-checker  "")
                   ('not-checked "=")
                   ('errored   (kevin/insert-face-text "!" "tomato"))
                   ('interrupted (kevin/insert-face-text "." "tomato"))
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

;; minor mode list
(defun kevin/minor-mode-line ()
  (when (> (window-width) 30)
    minor-mode-alist))

;; nyan progressbar
(defun kevin/nyan-mode-line ()
  (when (> (window-width) 150)
    '(:eval
      (list (nyan-create)))))

(defun kevin/position-mode-line ()
  "Show line,column and position info."
  ;; '%02' to set to 2 chars at least; prevents flickering
  (format "(%s)" (propertize "%02l:%02c")))

(defun kevin/time-mode-line ()
  (propertize (format-time-string "%H:%M")
              'help-echo
              (concat (format-time-string "%c; ")
                      (emacs-uptime "Uptime:%hh")))
  )

(defun kevin/anzu-mode-line ()
  (when (bound-and-true-p anzu-mode)
    (anzu--update-mode-line)
    ))

;; 自定义mode-line样式
;; anzu 必须加载后才可以设置 anzu--mode-line-format
(setq-default mode-line-format
              (list
               '(:eval
                 (let* (
                        (active (kevin/selected-window-active))
                        (face0 (if active 'mode-line 'mode-line-inactive))
                        (face1 (if active 'mode-line 'mode-line-inactive))
                        ;; (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                        ;; (mode-line (if active 'mode-line 'mode-line-inactive))
                        (lhs (list
                              evil-mode-line-tag
                              " "
                              (kevin/directory-mode-line face0 'l)
                              ;; (kevin/add-faicon-icon)
                              (kevin/vc-mode-line face0 'l)
                              " "
                              (kevin/anzu-mode-line)
                              " "
                              (kevin/flycheck-mode-line)
                              ;; minor-mode-list
                              ;; (kevin/minor-mode-line)


                              ))
                        (rhs (list
                              (kevin/position-mode-line)
                              (kevin/time-mode-line)
                              ))
                        )
                   (concat
                    (powerline-render lhs)
                    (mode-line-fill 'mode-line (powerline-width rhs))
                    (powerline-render rhs)
                    )
                   ))
               ))
;; (list
;;  '(:eval evil-mode-line-tag)
;;  " "
;;  ;; (kevin/window-number-mode-line)
;;  ;; (kevin/persp-mode-line)

;;  " "
;;  (kevin/vc-mode-line)

;;  ;; (kevin/buffer-state-info)
;;  (kevin/anzu-mode-line)
;;  (kevin/flycheck-mode-line)
;;  " "
;;  (kevin/minor-mode-line)


;; "["
;; ;; the current major mode for the buffer.
;; '(:eval (propertize (kevin/simplify-major-mode-name) 'face 'font-lock-string-face
;;                     'help-echo buffer-file-coding-system))
;; "]"

;; ;; global-mode-string goes in mode-line-misc-info
;; (mode-line-misc-info)
;; '(:eval (when (> (window-width) 120)
;;           mode-line-misc-info))

;; ;; encoding abbrev
;; "["
;; '(:eval (kevin/buffer-encoding-abbrev))
;; "]"

;; " "
;; ;; add the time, with the date and the emacs uptime in the tooltip
;; '(:eval (propertize (format-time-string "%H:%M")
;;                     'help-echo
;;                     (concat (format-time-string "%c; ")
;;                             (emacs-uptime "Uptime:%hh"))))

(provide 'init-modeline)
;;; init-modeline ends here
