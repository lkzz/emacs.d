;;; init-ivy.el --- ivy config. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
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
  (my-space-leader-def
    "s /" 'counsel-rg
    "s s" 'swiper-all)
  ("C-s"     'swiper
   "C-S-s"   'swiper-all
   "C-c C-r" 'ivy-resume
   "C-c t c" 'ivy-toggle-calling)
  (counsel-mode-map [remap swiper] 'counsel-grep-or-swiper
                    [remap swiper-backward] 'counsel-grep-or-swiper-backward
                    [remap dired] 'counsel-dired
                    [remap find-file] 'counsel-find-file
                    [remap recentf] 'counsel-recentf
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
  :init
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
        counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n"
        ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful " "\\`\\*.+-posframe-buffer\\*")
        counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  ;; Record in jumplist when opening files via counsel-{ag,rg,pt,git-grep}
  (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)
  (add-hook 'counsel-grep-post-action-hook #'recenter)

  ;; when swiper-action-recenter non-nil, frame blink in terminal
  (when (display-graphic-p)
    (setq swiper-action-recenter nil))

  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' '%s'"))
  (when (executable-find "fd")
    (setq counsel-fzf-cmd "fd --type f --hidden --follow --exclude .git --color never '%s'"))
  ;; Be compatible with `gls'
  (when (and is-mac-p (executable-find "gls"))
    (setq counsel-find-file-occur-use-find nil
          counsel-find-file-occur-cmd "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))
  :config
  ;; Highlight the selected item
  (defun my-ivy-format-function (cands)
    "Transform CANDS into a string for minibuffer."
    (if (display-graphic-p)
        (ivy-format-function-line cands)
      (ivy-format-function-arrow cands)))
  (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function)

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
    :general (my-space-leader-def "SPC" 'amx))

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
    :init
    (setq all-the-icons-ivy-rich-icon-size 0.85)
    (all-the-icons-ivy-rich-mode))

  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :hook (;; Must load after `counsel-projectile'
           (counsel-projectile-mode . ivy-rich-mode)
           (ivy-rich-mode . ivy-rich-project-root-cache-mode)
           (ivy-rich-mode . (lambda ()
                              "Use abbreviate in `ivy-rich-mode'."
                              (setq ivy-virtual-abbreviate
                                    (or (and ivy-rich-mode 'abbreviate) 'name)))))
    :init
    ;; For better performance
    (setq ivy-rich-parse-remote-buffer nil))

  (use-package prescient
    :config
    (setq prescient-history-length 2000
          prescient-filter-method '(literal regexp))
    (prescient-persist-mode 1))

  (use-package ivy-prescient
    :commands ivy-prescient-re-builder
    :custom-face
    (ivy-minibuffer-match-face-1 ((t (:foreground ,(face-foreground 'font-lock-doc-face nil t)))))
    :init
    (defun ivy-prescient-non-fuzzy (str)
      "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
      (let ((prescient-filter-method '(literal regexp)))
        (ivy-prescient-re-builder str)))

    (setq ivy-prescient-retain-classic-highlighting t
          ivy-re-builders-alist
          '((counsel-ag . ivy-prescient-non-fuzzy)
            (counsel-rg . ivy-prescient-non-fuzzy)
            (counsel-pt . ivy-prescient-non-fuzzy)
            (counsel-grep . ivy-prescient-non-fuzzy)
            (counsel-fzf . ivy-prescient-non-fuzzy)
            (counsel-imenu . ivy-prescient-non-fuzzy)
            (counsel-yank-pop . ivy-prescient-non-fuzzy)
            (swiper . ivy-prescient-non-fuzzy)
            (swiper-isearch . ivy-prescient-non-fuzzy)
            (swiper-all . ivy-prescient-non-fuzzy)
            (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
            (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
            (insert-char . ivy-prescient-non-fuzzy)
            (counsel-unicode-char . ivy-prescient-non-fuzzy)
            (t . ivy-prescient-re-builder))
          ivy-prescient-sort-commands
          '(:not swiper swiper-isearch ivy-switch-buffer
            lsp-ivy-workspace-symbol ivy-resume ivy--restore-session
            counsel-grep counsel-git-grep counsel-rg counsel-ag
            counsel-ack counsel-fzf counsel-pt counsel-imenu
            counsel-org-capture counsel-outline counsel-org-goto
            counsel-load-theme counsel-yank-pop
            counsel-recentf counsel-buffer-or-recentf))

    (ivy-prescient-mode 1)))

(provide 'init-ivy)
;;; init-ivy.el ends here
