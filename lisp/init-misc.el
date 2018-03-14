;;; init-misc.el --- misc config files
;;; Commentary:
;;; Code:

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :defer t
  :diminish flyspell-mode
  :init (setq flyspell-issue-message-flag nil))

;; Elec pair
(use-package elec-pair
  :ensure nil
  :defer t
  :init (add-hook 'after-init-hook #'electric-pair-mode))

;; Hungry deletion
(use-package hungry-delete
  :ensure t
  :defer t
  :diminish hungry-delete-mode
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode)
  ;; :config (setq-default hungry-delete-chars-to-skip " \t\f\v")
  )

(use-package server
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'server-start t))

(use-package restart-emacs
  :ensure t
  :bind (("C-x C-c" . restart-emacs)))

;; History
(use-package saveplace
  :ensure nil
  :defer t
  :init
  ;; Emacs 25 has a proper mode for `save-place'
  (if (fboundp 'save-place-mode)
      (add-hook 'after-init-hook #'save-place-mode)
    (setq save-place t)))

(use-package recentf
  :ensure nil
  :defer t
  :init
  (setq recentf-max-saved-items 100)
  ;; lazy load recentf
  ;; (add-hook 'after-init-hook #'recentf-mode)
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                     (recentf-mode)
                                     (recentf-track-opened-file))))
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude kevin/cache-directory))

(use-package savehist
  :ensure nil
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 60)
  (add-hook 'after-init-hook #'savehist-mode))

(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

;; Delete selection if you insert
(use-package delsel
  :defer t
  :init (add-hook 'after-init-hook #'delete-selection-mode))

;; Rectangle
(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :defer t
  :init
  (add-hook 'text-mode-hook #'goto-address-mode)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode))

;; Jump to things in Emacs tree-style
(use-package avy
  :ensure t
  :commands (avy-goto-char avy-goto-char-2)
  :init (add-hook 'after-init-hook #'avy-setup-default)
  :config (setq avy-background t))

;; Kill text between the point and the character CHAR
(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; Quickly follow links
(use-package ace-link
  :ensure t
  :bind (("M-o" . ace-link-addr))
  :init (add-hook 'after-init-hook #'ace-link-setup-default))

;; Jump to Chinese characters
(use-package ace-pinyin
  :ensure t
  :diminish ace-pinyin-mode
  :init (add-hook 'after-init-hook #'ace-pinyin-global-mode))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :init
  (add-hook 'after-init-hook #'global-aggressive-indent-mode)

  ;; FIXME: Disable in big files due to the performance issues
  ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
  (add-hook 'find-file-hook
            (lambda ()
              (if (> (buffer-size) (* 3000 80))
                  (aggressive-indent-mode -1))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode robot-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-mode)
             (derived-mode-p 'java-mode)
             (derived-mode-p 'go-mode)
             (derived-mode-p 'swift-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :ensure t
  :defer t
  :diminish drag-stuff-mode
  :init (add-hook 'after-init-hook #'drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :defer t
  :init
  ;; show org ediffs unfolded
  (with-eval-after-load 'outline
    (add-hook 'ediff-prepare-buffer-hook #'show-all))
  ;; restore window layout when done
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook #'winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :ensure t
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :init
  ;; Avoid to restore Iedit mode when restoring desktop
  (add-to-list 'desktop-minor-mode-handlers
               '(iedit-mode . nil)))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure t
  :bind (("C-." . imenu)))

;; Treat undo history as a tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :defer t
  :diminish subword-mode
  :init
  (add-hook 'prog-mode-hook #'subword-mode)
  (add-hook 'minibuffer-setup-hook #'subword-mode))

;; Hideshow
(use-package hideshow
  :ensure nil
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding))
  :diminish hs-minor-mode)

;; Move to the beginning/end of line or code
(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(provide 'init-misc)
;;; init-misc.el ends here
