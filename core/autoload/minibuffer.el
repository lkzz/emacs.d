;;;###autoload
(defun my/consult-set-evil-search-pattern (&optional condition)
  (let ((re
         (cond
          ((eq condition 'rg) (substring (car consult--grep-history) 1)) ;; HACK: assume the history begins with `#'
          ((or t (eq condition 'line)) (car consult--line-history)))))
    (add-to-history 'evil-ex-search-history re)
    (setq evil-ex-search-pattern (list re t t))
    (setq evil-ex-search-direction 'forward)
    (anzu-mode t)))

;;;###autoload
(defun my/consult-line-symbol-at-point ()
  (interactive)
  (evil-without-repeat ;; I use evil always
    (consult-line (thing-at-point 'symbol))
    (my/consult-set-evil-search-pattern)))

;;;###autoload
(defun my/consult-ripgrep-at-point (&optional dir initial)
  (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                  (symbol-name s))))
  (consult-ripgrep dir initial))
