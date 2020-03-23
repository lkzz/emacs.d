
;;;###autoload
(defun kevin/disable-highlight-indent-guides ()
  (when highlight-indent-guides-mode
    (highlight-indent-guides-mode -1)))
