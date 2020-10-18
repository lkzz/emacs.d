;;; init-ivy.el --- ivy config. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2020  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :general
  (kevin/space-key-define
    "f f" 'counsel-find-file
    "f r" 'counsel-recentf
    "s /" 'counsel-rg
    "s s" 'swiper-all)
  ("C-s"     'swiper
   "C-S-s"   'swiper-all
   "C-c C-r" 'ivy-resume
   "C-c t c" 'ivy-toggle-calling)
  (counsel-mode-map [remap swiper] 'counsel-grep-or-swiper
                    [remap swiper-backward] 'counsel-grep-or-swiper-backward
                    [remap dired] 'counsel-dired
                    [remap amx] 'counsel-M-x
                    [remap switch-to-buffer] 'counsel-switch-buffer
                    "C-x j" 'counsel-mark-ring
                    "C-c B" 'counsel-bookmarked-directory
                    "C-c L" 'counsel-load-library
                    "C-c O" 'counsel-find-file-extern
                    "C-c P" 'counsel-package
                    "C-c g" 'counsel-grep
                    "C-c h" 'counsel-command-history
                    "C-c r" 'counsel-rg
                    "C-c z" 'counsel-fzf)
  (ivy-minibuffer-map "C-w" 'ivy-yank-word
                      [escape] 'minibuffer-keyboard-quit)
  (counsel-find-file-map "C-h" 'counsel-up-directory
                         "C-l" 'counsel-down-directory)
  (swiper-map "M-%" 'swiper-query-replace)
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function nil
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t
        ivy-format-function #'ivy-format-function-arrow
        ivy-count-format "(%d/%d) "
        counsel-find-file-at-point t)
  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
    (when (and is-mac-p (executable-find "gls"))
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd
            "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))
  ;; Pre-fill search keywords
  ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
  (defvar my-ivy-fly-commands
    '(query-replace-regexp
      flush-lines keep-lines ivy-read
      swiper swiper-backward swiper-all
      swiper-isearch swiper-isearch-backward
      lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol
      counsel-grep-or-swiper counsel-grep-or-swiper-backward
      counsel-grep counsel-ack counsel-ag counsel-rg counsel-pt))
  (defvar-local my-ivy-fly--travel nil)

  ;; Display an arrow with the selected item
  (defun my-ivy-format-function-arrow (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat (if (and (bound-and-true-p all-the-icons-ivy-rich-mode)
                        (>= (length str) 1)
                        (string= " " (substring str 0 1)))
                   ">"
                 "> ")
               (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (if (and (bound-and-true-p all-the-icons-ivy-rich-mode)
                        (>= (length str) 1)
                        (string= " " (substring str 0 1)))
                   " "
                 "  ")
               str))
     cands
     "\n"))
  (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function-arrow)

  (defun my-ivy-fly-back-to-present ()
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((or (memq this-command '(self-insert-command
                                    ivy-forward-char
                                    ivy-delete-char delete-forward-char
                                    end-of-line mwim-end-of-line
                                    mwim-end-of-code-or-line mwim-end-of-line-or-code
                                    yank ivy-yank-word counsel-yank-pop))
               (equal (this-command-keys-vector) (kbd "M-n")))
           (unless my-ivy-fly--travel
             (delete-region (point) (point-max))
             (when (memq this-command '(ivy-forward-char
                                        ivy-delete-char delete-forward-char
                                        end-of-line mwim-end-of-line
                                        mwim-end-of-code-or-line
                                        mwim-end-of-line-or-code))
               (insert (ivy-cleanup-string ivy-text))
               (when (memq this-command '(ivy-delete-char delete-forward-char))
                 (beginning-of-line)))
             (setq my-ivy-fly--travel t)))))

  (defun my-ivy-fly-time-travel ()
    (when (memq this-command my-ivy-fly-commands)
      (let* ((kbd (kbd "M-n"))
             (cmd (key-binding kbd))
             (future (and cmd
                          (with-temp-buffer
                            (when (ignore-errors
                                    (call-interactively cmd) t)
                              (buffer-string))))))
        (when future
          (save-excursion
            (insert (propertize (replace-regexp-in-string
                                 "\\\\_<" ""
                                 (replace-regexp-in-string
                                  "\\\\_>" ""
                                  future))
                                'face 'shadow)))
          (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

  (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)
  (add-hook 'minibuffer-exit-hook (lambda ()
                                    (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))

  ;; An alternative M-x interface for Emacs
  (use-package amx
    :init (setq amx-history-length 10)
    :general (kevin/space-key-define "SPC" 'amx))

  ;; when swiper-action-recenter non-nil, frame blink in terminal
  (if (display-graphic-p)
      (setq swiper-action-recenter t)
    (setq swiper-action-recenter nil))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook (counsel-mode . counsel-projectile-mode)
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  ;; More friendly interface for ivy
  (use-package all-the-icons-ivy-rich
    :if (display-graphic-p)
    :init (setq all-the-icons-ivy-rich-icon-size 0.85)
    (all-the-icons-ivy-rich-mode))

  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :hook (;; Must load after `counsel-projectile'
           (counsel-projectile-mode . ivy-rich-mode)
           (ivy-rich-mode . (lambda ()
                              "Use abbreviate in `ivy-rich-mode'."
                              (setq ivy-virtual-abbreviate
                                    (or (and ivy-rich-mode 'abbreviate) 'name)))))
    :init
    ;; For better performance
    (setq ivy-rich-parse-remote-buffer nil))

  (use-package prescient
    :init
    (setq prescient-history-length 2000
          prescient-filter-method '(literal regexp))
    :config
    (prescient-persist-mode 1))

  (use-package ivy-prescient
    :config
    (setq ivy-prescient-sort-commands
          '(:not counsel-grep
                 counsel-rg
                 counsel-switch-buffer
                 ivy-switch-buffer
                 swiper
                 swiper-multi))
    (setq ivy-prescient-retain-classic-highlighting t
          ivy-prescient-enable-filtering nil
          ivy-prescient-enable-sorting t)
    (ivy-prescient-mode 1))

  )

(provide 'init-ivy)
;;; init-ivy.el ends here
