;;; core-functions.el -- functions used in emacs configurations. -*- lexical-binding: t; -*-
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

;;;;;;;;;;;;;;;;;;;;;;;;;; functions adjust from spacemacs ;;;;;;;;;;;;;;;;;;;;;;;

;;; Generated autoloads from init-funcs.el
;;;###autoload
(defun kevin/declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command."
  (let* ((command name)
         (full-prefix (concat kevin-leader-key " " prefix))
         (full-prefix-emacs (concat kevin-emacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         (full-prefix-emacs-lst (listify-key-sequence
                                 (kbd full-prefix-emacs))))
    ;; define the prefix command only if it does not already exist
    (unless long-name (setq long-name name))
    (which-key-add-key-based-replacements
      full-prefix-emacs (cons name long-name)
      full-prefix (cons name long-name))))

;;;***##autoload
(defun kevin/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`kevin-leader-key' and `kevin-emacs-leader-key'.
KEY should be a string suitable for passing to `kbd', and it
should not include the leaders. DEF is most likely a quoted
command. See `define-key' for more information about the possible
choices for DEF. This function simply uses `define-key' to add
the bindings.

For convenience, this function will accept additional KEY DEF
pairs. For example,

\(kevin/set-leader-keys
   \"a\" 'command1
   \"C-c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key kevin-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

;;;###autoload
(defun kevin/declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let  ((command (intern (concat (symbol-name mode) name)))
         (full-prefix (concat kevin-leader-key " " prefix))
         (full-prefix-emacs (concat kevin-emacs-leader-key " " prefix))
         (is-major-mode-prefix (string-prefix-p "m" prefix))
         (major-mode-prefix (concat kevin-major-mode-leader-key
                                    " " (substring prefix 1)))
         (major-mode-prefix-emacs
          (concat kevin-major-mode-emacs-leader-key
                  " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-add-major-mode-key-based-replacements mode
        full-prefix-emacs prefix-name
        full-prefix prefix-name)
      (when (and is-major-mode-prefix kevin-major-mode-leader-key)
        (which-key-add-major-mode-key-based-replacements mode major-mode-prefix prefix-name))
      (when (and is-major-mode-prefix kevin-major-mode-emacs-leader-key)
        (which-key-add-major-mode-key-based-replacements
          mode major-mode-prefix-emacs prefix-name)))))

;;;###autoload
(defun kevin/set-leader-keys-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`kevin-major-mode-leader-key' and
`kevin-major-mode-emacs-leader-key' for the major-mode
MODE. MODE should be a quoted symbol corresponding to a valid
major mode. The rest of the arguments are treated exactly like
they are in `kevin/set-leader-keys'."
  (let* ((map (intern (format "kevin-%s-map" mode))))
    (when (kevin//init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))

;;;###autoload
(defun kevin//acceptable-leader-p (key)
  "Return t if key is a string and non-empty."
  (and (stringp key) (not (string= key ""))))

;;;###autoload
(defun kevin//init-leader-mode-map (mode map &optional minor)
  "Check for MAP-prefix. If it doesn't exist yet, use `bind-map'
to create it and bind it to `kevin-major-mode-leader-key'
and `kevin-major-mode-emacs-leader-key'. If MODE is a
minor-mode, the third argument should be non nil."
  (let* ((prefix (intern (format "%s-prefix" map)))
         (leader1 (when (kevin//acceptable-leader-p
                         kevin-major-mode-leader-key)
                    kevin-major-mode-leader-key))
         (leader2 (when (kevin//acceptable-leader-p
                         kevin-leader-key)
                    (concat kevin-leader-key " m")))
         (emacs-leader1 (when (kevin//acceptable-leader-p
                               kevin-major-mode-emacs-leader-key)
                          kevin-major-mode-emacs-leader-key))
         (emacs-leader2 (when (kevin//acceptable-leader-p
                               kevin-emacs-leader-key)
                          (concat kevin-emacs-leader-key " m")))
         (leaders (delq nil (list leader1 leader2)))
         (emacs-leaders (delq nil (list emacs-leader1 emacs-leader2))))
    (or (boundp prefix)
        (progn
          (eval
           `(bind-map ,map
              :prefix-cmd ,prefix
              ,(if minor :minor-modes :major-modes) (,mode)
              :keys ,emacs-leaders
              :evil-keys ,leaders
              :evil-states (normal motion visual evilified)))
          (boundp prefix)))))


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
       (with-eval-after-load 'bind-map
         (kevin/set-leader-keys-for-major-mode ',mode
                                               "gg" 'kevin/jump-to-definition
                                               "gG" 'kevin/jump-to-definition-other-window)))))

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
(defun kevin/goto-match-parent ()
  "Go to the matching  if on (){}[], similar to vi style of %."
  (interactive)
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

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

(byte-recompile-file "~/.emacs.d/core/core-functions.el" nil 0)
(provide 'core-functions)
;;; core-functions.el ends here
