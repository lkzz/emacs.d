;;; init-minibuffer.el --- minibuffer config. -*- lexical-binding: t -*-
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
;;; Code:

(use-package minibuffer
  :straight (:type built-in)
  :bind (:map minibuffer-local-map
              ([escape] . abort-recursive-edit)
              :map minibuffer-local-ns-map
              ([escape] . abort-recursive-edit)
              :map minibuffer-local-completion-map
              ([escape] . abort-recursive-edit)
              :map minibuffer-local-must-match-map
              ([escape] . abort-recursive-edit)
              :map minibuffer-local-isearch-map
              ([escape] . abort-recursive-edit))
  :custom
  (completion-auto-help t)
  (completion-show-help nil)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)
  ;; Allow commands in minibuffers
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-eldef-shorten-default t)
  (minibuffer-electric-default-mode t)
  ;; One frame one minibuffer.
  (minibuffer-follows-selected-frame nil)
  ;; Ignore cases when complete
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; `selectrum', `vertico' and `icomplete' will honoring
  (completion-styles '(basic partial-completion substring flex))
  (completion-category-overrides '((buffer (styles . (flex)))))
  ;; vertical view
  (completions-format 'one-column)
  (completions-detailed t)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package vertico
  :straight (vertico :includes (vertico-quick vertico-repeat vertico-directory)
                     :files (:defaults "extensions/vertico-*.el"))
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle nil)

  ;; Quick action
  (use-package vertico-quick
    :after vertico
    :ensure nil
    :bind (:map vertico-map
                ("C-j" . vertico-quick-jump)
                ("C-i" . vertico-quick-insert)
                ("C-q" . vertico-quick-exit)))
  ;; Repeat last session
  (use-package vertico-repeat
    :after vertico
    :ensure nil
    :bind ("C-c C-r" . vertico-repeat)
    :config
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))
  ;;  Ido-like directory navigation
  (use-package vertico-directory
    :after vertico
    :ensure nil
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("C-w" . vertico-directory-delete-word))
    ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
    ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package marginalia
  :after vertico
  :hook (vertico-mode . marginalia-mode))

(use-package all-the-icons-completion
  :if (display-graphic-p)
  :after all-the-icons marginalia
  :straight (:host github :repo "iyefrat/all-the-icons-completion")
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight (:type built-in)
  :hook (my-first-input . savehist-mode)
  :init
  (setq history-length 200
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables '(kill-ring                         ; persist clipboard
                                        register-alist                    ; persist macros
                                        mark-ring global-mark-ring        ; persist marks
                                        search-ring regexp-search-ring))) ; persist searches

(use-package consult
  :bind (([remap isearch-forward]               . consult-line)
         ([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap evil-show-marks]               . consult-mark)
         ([remap evil-show-registers]           . consult-register)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop))
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-project-root-function #'projectile-project-root)
  (consult-customize  consult-bookmark consult-recent-file
                      :preview-key nil
                      consult-theme consult-xref consult-buffer
                      consult-ripgrep consult-git-grep consult-grep
                      my/consult-ripgrep-at-point
                      :preview-key '(:debounce 0.5 any)))

  (use-package consult-dir
    :bind (([remap list-directory] . consult-dir)
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :bind  (([remap describe-bindings] . embark-bindings)
          ("C-;" . embark-act))         ;; pick some comfortable binding
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  :config
  ;; From the embark wiki
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (caar targets) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t))))

  ;; Embark indicators
  (setq embark-indicators '(embark-which-key-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))
  (setq embark-verbose-indicator-display-action
        '(display-buffer-at-bottom
          (window-height . (lambda (win) (fit-window-to-buffer
                                          win (floor (frame-height)
                                                     3))))))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after embark consult)

;; Writable `grep' buffer
(use-package wgrep
  :hook (grep-setup . wgrep-setup)
  :config (setq wgrep-auto-save-buffer t))

;; HACK: Filter boring message in echo area.
(defadvice message (around my-message-filter activate)
  (unless (string-match "gofmt\\|skipped\\|tsc-dyn-get" (or (ad-get-arg 0) ""))
    ad-do-it))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
