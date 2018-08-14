;;; init-eshell.el --- config eshell. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(use-package company-shell
  :defer t
  :after company
  :config
  (progn
    (add-hook eshell-mode-hook (lambda ()
                                 (make-local-variable 'company-backends)
                                 (add-to-list 'company-backends '(company-backends
                                                                  company-keywords))))))

(use-package eshell-prompt-extras
  :defer t
  :commands epe-theme-lambda
  :init
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package shell-pop
  :defer t
  :commands shell-pop
  :defer t
  :init
  (progn
    ;; set shell-pop to use eshell
    ;; (setq-default shell-pop-shell-type '("eshell" "*eshell-pop*" (lambda nil (eshell))))
    (setq-default shell-pop-shell-type '("eshell" "*eshell*" (lambda nil (eshell))))
    ;; set the shell popup height
    (setq-default shell-pop-window-height 50)
    ;; set the shell popup to span the entire frame width(still has bug)
    ;; issue: https://github.com/syl20bnr/spacemacs/issues/7446
    (setq-default shell-pop-full-span nil)
    ;; pop the shell from the bottom of the frame
    (setq-default shell-pop-window-position "bottom")))

;; Eshell prompt for git users
(use-package eshell-git-prompt
  :defer t
  :init
  (add-hook 'eshell-load-hook
            (lambda () (eshell-git-prompt-use-theme "robbyrussell"))))

;; cd to frequent directory in eshell
(use-package eshell-z
  :defer t
  :init (add-hook 'eshell-mode-hook
                  (lambda () (require 'eshell-z))))

;; Emacs command shell
(use-package eshell
  :defer t
  :init
  (progn
    (setq eshell-cmpl-cycle-completions nil
          ;; auto truncate after 20k lines
          eshell-buffer-maximum-lines 1000
          ;; history size
          eshell-history-size 50
          ;; no duplicates in history
          eshell-hist-ignoredups t
          ;; buffer shorthand -> echo foo > #'buffer
          eshell-buffer-shorthand t
          ;; my prompt is easy enough to see
          eshell-highlight-prompt t
          ;; treat 'echo' like shell echo
          eshell-plain-echo-behavior t
          eshell-list-files-after-cd t
          eshell-banner-message ""
          ;; cache directory
          eshell-directory-name (concat kevin/cache-directory "eshell")
          eshell-visual-subcommands '(("git" "log" "diff" "show"))))
  :config
  (progn
    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
        (erase-buffer)))

    (defun eshell/emacs (&rest args)
      "Open a file (ARGS) in Emacs.  Some habits die hard."
      (if (null args)
          ;; If I just ran "emacs", I probably expect to be launching
          ;; Emacs, which is rather silly since I'm already in Emacs.
          ;; So just pretend to do what I ask.
          (bury-buffer)
        ;; We have to expand the file names or else naming a directory in an
        ;; argument causes later arguments to be looked for in that directory,
        ;; not the starting directory
        (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

    (defalias 'eshell/e 'eshell/emacs)

    (defun eshell/ec (&rest args)
      "Compile a file (ARGS) in Emacs.  Use `compile' to do background make."
      (if (eshell-interactive-output-p)
          (let ((compilation-process-setup-function
                 (list 'lambda nil
                       (list 'setq 'process-environment
                             (list 'quote (eshell-copy-environment))))))
            (compile (eshell-flatten-and-stringify args))
            (pop-to-buffer compilation-last-buffer))
        (throw 'eshell-replace-command
               (let ((l (eshell-stringify-list (eshell-flatten-list args))))
                 (eshell-parse-command (car l) (cdr l))))))
    (put 'eshell/ec 'eshell-no-numeric-conversions t)

    (defun eshell-view-file (file)
      "View FILE.  A version of `view-file' which properly rets the eshell prompt."
      (interactive "fView file: ")
      (unless (file-exists-p file) (error "%s does not exist" file))
      (let ((buffer (find-file-noselect file)))
        (if (eq (with-current-buffer buffer (get major-mode 'mode-class))
                'special)
            (progn
              (switch-to-buffer buffer)
              (message "Not using View mode because the major mode is special"))
          (let ((undo-window (list (window-buffer) (window-start)
                                   (+ (window-point)
                                      (length (funcall eshell-prompt-function))))))
            (switch-to-buffer buffer)
            (view-mode-enter (cons (selected-window) (cons nil undo-window))
                             'kill-buffer)))))

    (defun eshell/less (&rest args)
      "Invoke `view-file' on a file (ARGS).  \"less +42 foo\" will go to line 42 in the buffer for foo."
      (while args
        (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
            (let* ((line (string-to-number (match-string 1 (pop args))))
                   (file (pop args)))
              (eshell-view-file file)
              (forward-line line))
          (eshell-view-file (pop args)))))

    (defalias 'eshell/more 'eshell/less)

    (defun eshell/.. (&optional level)
      "Go up LEVEL directories"
      (interactive)
      (let ((level (or level 1)))
        (eshell/cd (make-string (1+ level) ?.))
        (eshell/ls)))

    (defun eshell/unpack (file)
      (let ((command (some (lambda (x)
                             (if (string-match-p (car x) file)
                                 (cadr x)))
                           '((".*\.tar.bz2" "tar xjf")
                             (".*\.tar.gz" "tar xzf")
                             (".*\.bz2" "bunzip2")
                             (".*\.rar" "unrar x")
                             (".*\.gz" "gunzip")
                             (".*\.tar" "tar xf")
                             (".*\.tbz2" "tar xjf")
                             (".*\.tgz" "tar xzf")
                             (".*\.zip" "unzip")
                             (".*\.Z" "uncompress")
                             (".*" "echo 'Could not unpack the file:'")))))
        (eshell-command-result (concat command " " file))))

    (defun +eshell/quit-or-delete-char (arg)
      (interactive "p")
      (if (and (eolp) (looking-back eshell-prompt-regexp nil))
          (eshell-life-is-too-much)
        (delete-char arg)))

    (defun my/ivy-eshell-history ()
      (interactive)
      (require 'em-hist)
      (let* ((start-pos (save-excursion (eshell-bol) (point)))
             (end-pos (point))
             (input (buffer-substring-no-properties start-pos end-pos))
             (command (ivy-read "Command: "
                                (delete-dups
                                 (when (> (ring-size eshell-history-ring) 0)
                                   (ring-elements eshell-history-ring)))
                                :initial-input input)))
        (setf (buffer-substring start-pos end-pos) command)
        (end-of-line)))

    (defun kevin/eshell-keymap ()
      (evil-define-key 'insert eshell-mode-map
        (kbd "C-p") 'eshell-previous-matching-input-from-input
        (kbd "C-n") 'eshell-next-matching-input-from-input
        (kbd "C-u") 'eshell-kill-input
        (kbd "C-a") 'eshell-bol
        (kbd "C-d") #'+eshell/quit-or-delete-char
        (kbd "C-r") #'my/ivy-eshell-history
        (kbd "<tab>") (lambda () (interactive) (pcomplete-std-complete))))
    (add-hook 'eshell-first-time-mode-hook #'kevin/eshell-keymap)

    (defun moon-validate-command ()
      "Validate eshell command."
      (save-excursion
        (beginning-of-line)
        (re-search-forward (format "%s\\([^ ]*\\)" eshell-prompt-regexp)
                           (line-end-position)
                           t)
        (let ((beg (match-beginning 1))
              (end (match-end 1))
              (command (match-string 1)))
          (put-text-property beg end
                             'face `(:foreground ,(if (executable-find command)
                                                      "#98C379"
                                                    "red"))))))

    (add-hook 'eshell-mode-hook (lambda ()
                                  "Add `moon-validate-command' to `post-command-hook'."
                                  (add-hook 'post-command-hook #'moon-validate-command t t)))

    ))

(use-package esh-autosuggest
  :after eshell
  :hook (eshell-mode . esh-autosuggest-mode)
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  ;; :hook (eshell-mode-hook . esh-autosuggest-mode)
  :ensure t)

(provide 'init-eshell)
;;; init-eshell.el ends here
