;; consult-fd
(defvar consult--fd-command nil)
(defun consult--fd-builder (input)
  (unless consult--fd-command
    (setq consult--fd-command
          (if (eq 0 (call-process-shell-command "fdfind"))
              "fdfind"
            "fd")))
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler
                                      arg 'extended t)))
    (when re
      (list :command (append
                      (list consult--fd-command
                            "--color=never" "--full-path"
                            (consult--join-regexps re 'extended))
                      opts)
            :highlight hl))))

;;;###autoload
(defun my/consult-fd (&optional dir initial)
  (interactive "P")
  (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
         (default-directory (cdr prompt-dir)))
    (call-interactively #'find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))

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
