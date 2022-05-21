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
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)
         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)
         ([remap insert-char] . counsel-unicode-char)
         ([remap recentf-open-files] . counsel-recentf)
         ([remap org-capture] . counsel-org-capture)
         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)
         ([escape] . minibuffer-keyboard-quit)
         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)
         ("C-l" . counsel-down-directory)
         :map swiper-map
         ("M-s" . swiper-isearch-toggle)
         ("M-%" . swiper-query-replace)
         :map isearch-mode-map
         ("M-s" . swiper-isearch-toggle))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq ivy-height 12
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'abbreviate
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
  (add-hook 'counsel-grep-post-action-hook #'recenter)
  (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)

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
    (all-the-icons-ivy-rich-mode)
    :config
    (let ((my-recentf-transformer '(:columns ; TODO trancate long path string into one line
                                    ((all-the-icons-ivy-rich-file-icon)
                                     (all-the-icons-ivy-rich-file-name (:width 0.70))
                                     (all-the-icons-ivy-rich-file-modes (:width 10))
                                     (ivy-rich-file-last-modified-time (:witdh 0.15 :face all-the-icons-ivy-rich-time-face)))
                                    :delimiter "\t"))
          (my-bookmark-transformer '(:columns
                                     ((all-the-icons-ivy-rich-bookmark-icon)
                                      (all-the-icons-ivy-rich-bookmark-name (:width 0.15))
                                      (ivy-rich-bookmark-type (:width 10))
                                      (all-the-icons-ivy-rich-bookmark-filename (:width 0.50 :face all-the-icons-ivy-rich-bookmark-face))
                                      (all-the-icons-ivy-rich-bookmark-context (:face all-the-icons-ivy-rich-doc-face)))
                                     :delimiter "\t")))
      ;; counsel-recentf
      (plist-put all-the-icons-ivy-rich-display-transformers-list
                 'counsel-recentf my-recentf-transformer)
      ;; counsel-bookmark
      (plist-put all-the-icons-ivy-rich-display-transformers-list
                 'counsel-bookmark my-bookmark-transformer))
    ;; reload ivy-rich-mode to apply configuration
    (ivy-rich-mode -1)
    (ivy-rich-mode +1))

  ;; More friendly display transformer for Ivy
  ;; Must load after `counsel-projectile'
  (use-package ivy-rich
    :hook ((counsel-projectile-mode . ivy-rich-mode)
           (ivy-rich-mode . ivy-rich-project-root-cache-mode))
    :init
    (setq ivy-rich-path-style 'abbreviate)
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
