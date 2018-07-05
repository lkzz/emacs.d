;;; init-modeline.el --- modeline config for emacs.
;;; Commentary:
;;; Code:

(defun pl/render (item)
  "Render a powerline ITEM."
  (cond
   ((and (listp item) (eq 'image (car item)))
    (propertize " " 'display item
                'face (plist-get (cdr item) :face)))
   (item item)))

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
           (powerline-width (cdr values)))

        (message var)

        )
    0))

;; 显示layout
(defun kevin/update-persp-name ()
  (when (bound-and-true-p persp-mode)
    ;; There are multiple implementations of
    ;; persp-mode with different APIs
    (progn
      (or (not (string= persp-nil-name (safe-persp-name (get-frame-persp))))
          "Default")
      (let ((name (safe-persp-name (get-frame-persp))))
        (propertize (concat "[" name "] ")
                    'face 'font-lock-preprocessor-face
                    'help-echo "Current Layout name.")))))

(defconst kevin/flycheck-mode-line
  '(:eval
    (pcase flycheck-last-status-change
      ((\` not-checked) nil)
      ((\` no-checker) (propertize "- " 'face 'warning))
      ((\` running) (propertize "✷ " 'face 'success))
      ((\` errored) (propertize "! " 'face 'error))
      ((\` finished)
       (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
              (no-errors (cdr (assq 'error error-counts)))
              (no-warnings (cdr (assq 'warning error-counts)))
              (face (cond (no-errors 'error)
                          (no-warnings 'warning)
                          (t 'success))))
         (propertize (format "[%s/%s] " (or no-errors 0) (or no-warnings 0))
                     'face face)))
      ((\` interrupted) "- ")
      ((\` suspicious) '(propertize "? " 'face 'warning)))))

;; 简化 major-mode 的名字，替换表中没有的显示原名
(defun kevin/simplify-major-mode-name ()
  "Return simplifyed major mode name."
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
    (if replace-name replace-name major-name
        )))


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


(defun window-number-mode-line ()
  "The current window number,requires `window-numbering-mode' to be enabled."
  (when (bound-and-true-p window-numbering-mode)
    (let* ((num (window-numbering-get-number))
           (str (when num (int-to-string num))))
      (spaceline--unicode-number str))))

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

(defvar kevin/buffer-file-name
  '(:propertize
    (:eval (if (buffer-file-name) (concat (shorten-directory default-directory 20) (file-relative-name buffer-file-name)) " "))
    'face mode-line-directory)
  "Formats the current directory.")

(defvar kevin/buffer-file-state
  (concat
   "[" ;; insert vs overwrite mode, input-method in a tooltip
   (propertize (if overwrite-mode "Ovr" "Ins")
               'face 'font-lock-preprocessor-face
               'help-echo (concat "Buffer is in "
                                  (if overwrite-mode
                                      "overwrite"
                                    "insert") " mode"))

   ;; was this buffer modified since the last save?
   (when (buffer-modified-p)
     (concat "," (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified")))
   ;; is this buffer read-only?
   (when buffer-read-only
     (concat "," (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only")))
   "] "
   ))

(defvar kevin/position-segment
  '(:propertize "(%l:%c) " 'face mode-line))

(defun kevin/timestamp-segment (&optional face)
  "Add the time, with the date and the emacs uptime in the tooltip."
  (unless face
    (setq face 'mode-line))
  (propertize (format-time-string "%H:%M ")
              'face face))

(defun kevin/buffer-encoding-abbrev-segment ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))



(defvar kevin/mode-line-lhs
  (list
   kevin/buffer-file-name
   kevin/buffer-file-state
   ))

(defvar kevin/mode-line-rhs
  (list
   (kevin/timestamp-segment)
   (kevin/buffer-encoding-abbrev-segment)
   ))

;; 自定义mode-line样式
;; anzu 必须加载后才可以设置 anzu--mode-line-format
(setq-default mode-line-format
              (list

               ;; evil state
               '(:eval evil-mode-line-tag)


               kevin/mode-line-lhs

               '(:eval (propertize
                        (window-number-mode-line)
                        'face
                        'font-lock-type-face))
               " "
               '(:eval (kevin/update-persp-name))

               (with-eval-after-load 'anzu
                 '(:eval (anzu--update-mode-line)))


               ;; relative position, size of file
               " ["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ;; "["
               ;; ;; the current major mode for the buffer.
               ;; '(:eval (propertize (kevin/simplify-major-mode-name) 'face 'font-lock-string-face
               ;;                     'help-echo buffer-file-coding-system))
               ;; "]"

               "%1"
               kevin/flycheck-mode-line
               "%1"

               " "
               ;; git info
               '(:eval (when (> (window-width) 120)
                         `(vc-mode vc-mode)))
               " "

               ;; ;; nyan progressbar
               ;; '(:eval (when (> (window-width) 150)
               ;;           (list (nyan-create))))

               ;; minor modes
               '(:eval (when (> (window-width) 90)
                         minor-mode-alist))

               '(:eval
                 (concat
                  (mode-line-fill 'mode-line-inactive (powerline-width kevin/mode-line-rhs))
                  (mode-line-fill 'mode-line-inactive (powerline-width kevin/position-segment))
                  (powerline-render kevin/mode-line-rhs)
                  ))

               ))


(provide 'init-modeline)
;;; init-modeline ends here
