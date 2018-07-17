;;; init-funcs.el. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;            functions used in emacs configurations
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;; functions copy from spacemacs ;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun kevin/declare-prefix (prefix name &optional long-name)
  "Declare a prefix PREFIX. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command."
  (let* ((command name)
         (full-prefix (concat kevin/leader-key " " prefix))
         (full-prefix-emacs (concat kevin/emacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         (full-prefix-emacs-lst (listify-key-sequence
                                 (kbd full-prefix-emacs))))
    ;; define the prefix command only if it does not already exist
    (unless long-name (setq long-name name))
    (which-key-add-key-based-replacements
      full-prefix-emacs (cons name long-name)
      full-prefix (cons name long-name))))
(put 'kevin/declare-prefix 'lisp-indent-function 'defun)

;;;###autoload
(defun kevin/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`kevin/leader-key' and `kevin/emacs-leader-key'.
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
    (define-key kevin/default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))
(put 'kevin/set-leader-keys 'lisp-indent-function 'defun)

(defalias 'evil-leader/set-key 'kevin/set-leader-keys)


;;;###autoload
(defun kevin/declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let  ((command (intern (concat (symbol-name mode) name)))
         (full-prefix (concat kevin/leader-key " " prefix))
         (full-prefix-emacs (concat kevin/emacs-leader-key " " prefix))
         (is-major-mode-prefix (string-prefix-p "m" prefix))
         (major-mode-prefix (concat kevin/major-mode-leader-key
                                    " " (substring prefix 1)))
         (major-mode-prefix-emacs
          (concat kevin/major-mode-emacs-leader-key
                  " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-add-major-mode-key-based-replacements mode
        full-prefix-emacs prefix-name
        full-prefix prefix-name)
      (when (and is-major-mode-prefix kevin/major-mode-leader-key)
        (which-key-add-major-mode-key-based-replacements mode major-mode-prefix prefix-name))
      (when (and is-major-mode-prefix kevin/major-mode-emacs-leader-key)
        (which-key-add-major-mode-key-based-replacements
          mode major-mode-prefix-emacs prefix-name)))))
(put 'kevin/declare-prefix-for-mode 'lisp-indent-function 'defun)

;;;###autoload
(defun kevin/set-leader-keys-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`kevin/major-mode-leader-key' and
`kevin/major-mode-emacs-leader-key' for the major-mode
MODE. MODE should be a quoted symbol corresponding to a valid
major mode. The rest of the arguments are treated exactly like
they are in `kevin/set-leader-keys'."
  (let* ((map (intern (format "kevin-%s-map" mode))))
    (when (kevin//init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings) def (pop bindings))))))
(put 'kevin/set-leader-keys-for-major-mode 'lisp-indent-function 'defun)

(defalias 'evil-leader/set-key-for-mode 'kevin/set-leader-keys-for-major-mode)

;;;###autoload
(defun kevin//acceptable-leader-p (key)
  "Return t if key is a string and non-empty."
  (and (stringp key) (not (string= key ""))))

;;;###autoload
(defun kevin//init-leader-mode-map (mode map &optional minor)
  "Check for MAP-prefix. If it doesn't exist yet, use `bind-map'
to create it and bind it to `kevin/major-mode-leader-key'
and `kevin/major-mode-emacs-leader-key'. If MODE is a
minor-mode, the third argument should be non nil."
  (let* ((prefix (intern (format "%s-prefix" map)))
         (leader1 (when (kevin//acceptable-leader-p
                         kevin/major-mode-leader-key)
                    kevin/major-mode-leader-key))
         (leader2 (when (kevin//acceptable-leader-p
                         kevin/leader-key)
                    (concat kevin/leader-key " m")))
         (emacs-leader1 (when (kevin//acceptable-leader-p
                               kevin/major-mode-emacs-leader-key)
                          kevin/major-mode-emacs-leader-key))
         (emacs-leader2 (when (kevin//acceptable-leader-p
                               kevin/emacs-leader-key)
                          (concat kevin/emacs-leader-key " m")))
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

;;;;;;;;;;;;;;;;;;;; functions copy from spacemacs ends here ;;;;;;;;;;;;;;;;;;;;



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
(defun kevin/go-enable-gometalinter ()
  "Enable `flycheck-gometalinter' and disable overlapping `flycheck' linters."
  (setq flycheck-disabled-checkers '(go-gofmt
                                     go-golint
                                     go-vet
                                     go-build
                                     go-test
                                     go-errcheck))
  (flycheck-gometalinter-setup))

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
(defun blog-example ()
  (interactive)
  (with-output-to-temp-buffer "*blog-example*"
    (shell-command "echo This is an example"
                   "*blog-example*"
                   "*Messages*")
    (pop-to-buffer "*blog-example*")))

;;;###autoload
(defun kevin/complie ()
  "Complie command."
  (interactive)
  (setf my-list '())
  (loop for x in (split-string (buffer-file-name) "/") do
        (message x)
        (append my-list x)
        ))

;;;###autoload
(defun kevin/open-init-file ()
  "Open emacs init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;;;###autoload
(defun kevin/insert-faicon-icon-with-text (icon text &optional color help-echo)
  "Retorna una cadena de texto formateada con `propertize' de un icono de all-the-icons"
  (concat
   (propertize (all-the-icons-faicon icon)
               'face `(:foreground ,(or color "gray") :height 1.0 :family ,(all-the-icons-faicon-family))
               'display '(raise -0.1)
               'help-echo help-echo)
   (propertize (format "%s" text)
               'face `(:foreground ,(or color "gray") :height 1.0)
               ;; 'face 'mode-line
               'display '(raise -0.0)
               )))

(provide 'init-funcs)
;;; init-funcs.el ends here
