;;; init-misc.el --- misc config files
;;; Commentary:
;;; Code:

;; Elec pair
(use-package elec-pair
  :ensure nil
  :defer t
  :init (add-hook 'after-init-hook #'electric-pair-mode))

;; Hungry deletion
(use-package hungry-delete
  :ensure t
  :defer t
  :diminish hungry-delete-mode "ⓗ"
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode))

(use-package restart-emacs
  :ensure t
  :defer t)

(use-package server
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'server-start t))

;; History
(use-package saveplace
  :ensure nil
  :defer t
  :init
  (add-hook 'after-init-hook #'save-place-mode))

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

;; Delete selection if you insert
(use-package delsel
  :defer t
  :init (add-hook 'after-init-hook #'delete-selection-mode))

;; Rectangle
(use-package rect
  :ensure nil
  :defer t
  :bind (("<C-return>" . rectangle-mark-mode)))

;; Jump to things in Emacs tree-style
(use-package avy
  :ensure t
  :defer t
  :commands (avy-goto-char avy-goto-word-or-subword-1)
  :hook (after-init . avy-setup-default)
  :init
  (progn
    (evil-leader/set-key
      "jc" 'avy-goto-char
      "jw" 'avy-goto-word-or-subword-1
      "jl" 'avy-goto-line
      "jp" #'kevin/goto-match-parent))
  :config (setq avy-background t))

;; Quickly follow links
(use-package ace-link
  :ensure t
  :defer t
  :bind (("M-o" . ace-link-addr))
  :init (add-hook 'after-init-hook #'ace-link-setup-default))

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
  :defer t
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

(use-package multiple-cursors
  :ensure t
  :defer t
  :commands (hydra-multiple-cursors/body)
  :init
  (progn
    (evil-leader/set-key "mc" #'hydra-multiple-cursors/body)
    (defhydra hydra-multiple-cursors (:hint nil)
      "
     ^Up^            ^Down^        ^Other^
    ----------------------------------------------
    [_p_]   Previous    [_n_]   Next    [_l_] Edit lines
    [_P_]   Skip        [_N_]   Skip    [_a_] Mark all
    [_M-p_] Unmark      [_M-n_] Unmark  [_r_] Mark by regexp
    ^ ^                 ^ ^             [_q_] Quit
    "
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this :exit t)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("r" mc/mark-all-in-region-regexp :exit t)
      ("q" nil))))

;; Treat undo history as a tree
(use-package undo-tree
  :ensure nil
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode))

;; Hideshow
(use-package hideshow
  :ensure nil
  :defer t
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding))
  :diminish hs-minor-mode)

;; Move to the beginning/end of line or code
(use-package mwim
  :ensure t
  :defer t)

(use-package wgrep
  :ensure t)

(provide 'init-misc)
;;; init-misc.el ends here
