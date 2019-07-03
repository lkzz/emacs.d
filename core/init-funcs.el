;;; init-funcs.el -- functions used in emacs configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;    refer: https://github.com/syl20bnr/spacemacs
;;; Code:


(defvar kevin-default-jump-handlers '()
  "List of jump handlers available in every mode.")

(defvar-local kevin-jump-handlers '()
  "List of jump handlers local to this buffer.")

(defmacro kevin/define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given MODE.
This defines a variable `kevin-jump-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `kevin-jump-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "kevin//init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "kevin-jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`kevin-default-jump-handlers'.")
                  mode))
       (defun ,func ()
         (setq kevin-jump-handlers
               (append ,handlers-list
                       kevin-default-jump-handlers)))
       (add-hook ',mode-hook ',func)
       (kevin/set-leader-keys-for-major-mode
         "gg" 'kevin/jump-to-definition
         "gG" 'kevin/jump-to-definition-other-window))))

(defun kevin/jump-to-definition ()
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler kevin-jump-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (message "No jump handler was able to find this symbol.")))

(defun kevin/jump-to-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `kevin/jump-to-definition' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (kevin/jump-to-definition)))

;; Set the `:jump' property manually instead of just using `evil-define-motion'
;; in an `eval-after-load' macro invocation because doing that prevents
;; `describe-function' from correctly finding the source.
;;
;; See discussion on https://github.com/syl20bnr/kevin/pull/6771
(with-eval-after-load 'evil
  (evil-set-command-property 'kevin/jump-to-definition :jump t))

;;;;;;;;;;;;;;;;;;;; functions adjust from spacemacs ends here ;;;;;;;;;;;;;;;;;;;;

;; applescript
;;;###autoload
(defun kevin/open-iterm ()
  "Open iTerm and focus on it."
  (interactive)
  (do-applescript
   "do shell script \"open -a iTerm\"\n"))

;;;###autoload
(defun kevin/open-wechat ()
  "Open WeChat and focus on it."
  (interactive)
  (do-applescript
   "do shell script \"open -a WeChat\"\n"))

;;;###autoload
(defun kevin/open-youdao ()
  "Open youdao dictionary and focus on it."
  (interactive)
  (do-applescript
   "do shell script \"open -a 有道词典\"\n"))

;;;###autoload
(defun kevin/open-chrome ()
  "Open chrome dictionary and focus on it."
  (interactive)
  (shell-command "open /Applications/Google\sChrome.app --args --enable-net-benchmarking"))

;;;###autoload
(defun kevin/goto-match-delimiter ()
  "Go to the matching  if on (){}[], similar to vi style of %."
  (interactive)
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

;; returns the enclosing character for the character "c"
(defun get-enc-char (c) (cond
                         ((string= c "(") ")")
                         ((string= c "[") "]")
                         ((string= c "{") "}")
                         ((string= c ">") "<")
                         ((string= c "<") ">")
                         ((string= c "'") "'")
                         ((string= c "\"") "\"")
                         (t nil)
                         ))
(defvar empty-enclose 0)

;;;###autoload
(defun kevin/delete-delimiter-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (setq empty-enclose 0)
  (save-excursion
    (let (p1 p2 orig)
      (setq orig (point))
      (setq p1 (point))
      (setq p2 (point))
      (setq find 0)
      (setq mychar (thing-at-point 'char))
      (if (-contains? '("(" "[" "{" "<" "'" "\"") mychar)
          (progn
            (setq left_encloser (thing-at-point 'char))
            (backward-char -1)
            (if (string-equal (thing-at-point 'char) (get-enc-char left_encloser))
                (progn
                  (backward-char -1)
                  (setq p2 (point))
                  (setq find 1)
                  (setq empty-enclose 1)))))
      (while (eq find 0)
        (skip-chars-backward "^({[<>\"'")
        (setq p1 (point))
        (backward-char 1)
        (setq left_encloser (thing-at-point 'char))
        (goto-char orig)
        (while (and (not (eobp)) (eq find 0))
          (backward-char -1)
          (skip-chars-forward "^)}]<>\"'")
          (setq right_encloser (thing-at-point 'char))
          (if (string-equal right_encloser (get-enc-char left_encloser))
              (progn
                (setq p2 (point))
                (setq find 1))))
        (goto-char p1)
        (backward-char 1))
      (delete-region p1 p2)))
  (if (eq empty-enclose 0)
      (backward-char 1)))

;;;###autoload
(defun kevin/buffer-too-big-p ()
  "Check if buffer size is larger than 1M or has more than 5000 lines."
  (or (> (buffer-size) (* 1024 1024))
      (> (line-number-at-pos (point-max)) 5000)))

;;;###autoload
(defun kevin/make-frame ()
  "New a frame,and erase buffer."
  (interactive)
  (make-frame)
  (kevin/create-scratch-buffer))

;;;###autoload
(defun kevin/bazel-update ()
  "Bazel update in go-common."
  (interactive)
  (message "bazel update start!")
  (cd (concat (getenv "GOPATH") "/src/go-common"))
  (shell-command "make update")
  (message "bazel update done!"))

;;;###autoload
(defun kevin/bazel-build ()
  "Bazel build in go-common."
  (interactive)
  (message "bazel build start!")
  (cd (concat (getenv "GOPATH") "/src/go-common"))
  (shell-command "make build")
  (message "bazel build done!"))

;;;###autoload
(defun kevin/open-init-file ()
  "Open emacs init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;;###autoload
(defun kevin/set-frame-transparency (value)
  "Set the transparency of the frame window.
Argument VALUE 0 is transparent, 100 is opaque."
  (interactive "nTransparency Value (0 - 100): ")
  (set-frame-parameter (selected-frame) 'alpha value))

;;;###autoload
(defun kevin/delete-word ()
  "Delete word under cursor."
  (interactive)
  (let ((end (get-point 'forward-word 1))
        (beg (get-point 'backward-word 1)))
    (delete-region beg end)))

;;;###autoload
(defun kevin/copy-word ()
  "print current word."
  (interactive)
  (kill-new (thing-at-point 'word)))

;;;###autoload
(defun kevin/cover-word ()
  "cover word before point"
  (interactive)
  (kevin/delete-word)
  (evil-paste-before 1))

;;;###autoload
(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point))

;;;###autoload
(defun kevin/goto-definition (&optional arg)
  "Goto definition and add bookmark at line."
  (interactive "P")
  (cl-case major-mode
    (go-mode (call-interactively 'godef-jump))
    (emacs-lisp-mode (elisp-def-mode 1) (call-interactively 'elisp-def))
    (lisp-interaction-mode (elisp-def-mode 1) (call-interactively 'elisp-def))
    (python-mode (call-interactively 'jedi:goto-definition))
    (otherwise
     (counsel-etags-find-tag-at-point)))
  (setq this-command 'kevin/goto-definition)
  )

(defun kevin/insert-elisp-file-header ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- insert description here -*- lexical-binding: t -*-\n"
              ";;\n"
              ";; Copyright (C) 2017-2019  Kevin Leung\n"
              ";;\n"
              ";; Author: Kevin Leung <kevin.scnu@gmail.com>\n"
              ";; URL: https://github.com/lkzz/emacs.d\n"
              ";;\n"
              ";; This file is not part of GNU Emacs.\n"
              ";;\n"
              ";;; License: GPLv3\n"
              ";;\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))

(defun kevin/enable-yasnippet-in-company (backend)
  (if (or (not kevin-enable-company-yasnippet)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defmacro kevin/add-company-backend (&rest props)
  "Add and enable company backends.Available PROPS:
                 `:backend BACKEND' One company backends.
                 `:mode MODE' One mode where BACKENDSwill be added."
  (let* ((backend (plist-get props :backend))
         (mode (plist-get props :mode))
         (backend-list-name (intern (format "company-backends-%S" mode)))
         (mode-hook-name (intern (format "%S-hook" mode)))
         (init-func-name (intern (format "kevin/init-company-%S" mode))))
    `(progn
       ;; declare buffer local company-backends variable
       (defvar ,backend-list-name kevin-company-default-backends
         ,(format "Company backend list for %S." mode))
       (add-to-list ',backend-list-name ',backend)
       ;; define company init hook function for mode
       (defun ,init-func-name ()
         ,(format "Init company backends for %S" mode)

         (if kevin-enable-company-yasnippet
             (setq ,backend-list-name
                   (mapcar 'kevin/enable-yasnippet-in-company
                           ,backend-list-name)))
         (set (make-variable-buffer-local 'company-backends)
              ,backend-list-name))

       (add-hook ',mode-hook-name ',init-func-name t))))



(provide 'init-funcs)
;;; init-funcs.el ends here
