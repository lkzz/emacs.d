;;; init-ui.el ---  setup ui. -*- lexical-binding: t; -*-
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

(use-package doom-themes
  :defer t
  :init
  (add-hook 'kevin-load-theme-hook #'doom-themes-org-config)
  (add-hook 'kevin-load-theme-hook #'doom-themes-neotree-config)
  (setq doom-dark+-blue-modeline t
        doom-themes-neotree-file-icons 'simple
        doom-themes-neotree-line-spacing 2))

(use-package solaire-mode
  :defer t
  :when (or (daemonp) (display-graphic-p))
  :custom-face
  (solaire-hl-line-face ((t (:inherit hl-line :background "#272a27"))))
  :init
  (add-hook 'kevin-load-theme-hook '(lambda ()
                                      (require 'solaire-mode)
                                      (solaire-mode-swap-bg)))
  :config
  ;; fringe can become unstyled when deleting or focusing frames
  (add-hook 'focus-in-hook #'solaire-mode-reset)
  ;; org-capture takes an org buffer and narrows it. The result is erroneously
  ;; considered an unreal buffer, so solaire-mode must be restored.
  (add-hook 'org-capture-mode-hook #'turn-on-solaire-mode)
  ;; Because fringes can't be given a buffer-local face, they can look odd, so
  ;; we remove them in the minibuffer and which-key popups (they serve no
  ;; purpose there anyway).
  (defun kevin/disable-fringes-in-minibuffer-h (&rest _)
    (set-window-fringes (minibuffer-window) 0 0 nil))

  (add-hook 'solaire-mode-hook #'kevin/disable-fringes-in-minibuffer-h)
  (add-hook 'minibuffer-setup-hook #'kevin/disable-fringes-in-minibuffer-h)
  (add-hook 'window-configuration-change-hook #'kevin/disable-fringes-in-minibuffer-h)

  (solaire-global-mode +1))

;; 加载主题
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (load-theme 'doom-one t)))
  (load-theme 'doom-one t))

;; 启动时默认最大化
(toggle-frame-maximized)

(use-package vi-tilde-fringe
  :if (fboundp 'set-fringe-mode)
  :diminish vi-tilde-fringe-mode
  :hook ((prog-mode text-mode conf-mode) . vi-tilde-fringe-mode))

;; config built-in "display-line-numbers-mode" (require Emacs >= 26)
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode conf-mode protobuf-mode) . display-line-numbers-mode)
  :init
  (setq-default display-line-numbers-width 3)
  (setq display-line-numbers-current-absolute t))

;; 设置时间格式
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-24hr-format t
        display-time-day-and-date t))

;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 0.9)
  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\go.mod$" all-the-icons-fileicon "go" :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\go.sum$" all-the-icons-fileicon "go" :face all-the-icons-dpurple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-playlist-mode all-the-icons-material "playlist_play" :height 1.2 :v-adjust -0.2 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :v-adjust -0.2 :face 'all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rss$" all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-list-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-item-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue)))

(use-package nyan-mode
  :if (display-graphic-p)
  :init
  (setq nyan-animate-nyancat nil)
  (nyan-mode t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (defun my-doom-modeline--font-height ()
    "Calculate the actual char height of the mode-line."
    (+ (frame-char-height) 2))
  (advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)
  (add-hook #'kevin-load-theme-hook #'doom-modeline-refresh-bars)
  (setq doom-modeline-bar-width 3
        doom-modeline-env-enable-python t
        doom-modeline-icon (display-graphic-p)
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-modal-icon t
        doom-modeline-indent-info nil
        doom-modeline-unicode-fallback t
        doom-modeline-enable-word-count t
        doom-modeline-vcs-max-length 20
        doom-modeline-buffer-encoding nil
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-buffer-file-name-style 'auto))

(use-package hide-mode-line
  :hook ((neotree-mode . hide-mode-line-mode)
         (dashboard-mode . hide-mode-line-mode)
         (dired-mode . hide-mode-line-mode)))


(use-package centaur-tabs
  :if (display-graphic-p)
  :commands centaur-tabs-select-visible-tab
  :hook ((dashboard-mode . centaur-tabs-local-mode)
         (term-mode . centaur-tabs-local-mode)
         (calendar-mode . centaur-tabs-local-mode)
         (org-agenda-mode . centaur-tabs-local-mode)
         (helpful-mode . centaur-tabs-local-mode))
  :init
  (global-set-key (kbd "s-1") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-2") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-3") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-4") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-5") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-6") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-7") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-8") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-9") 'centaur-tabs-select-visible-tab)
  (global-set-key (kbd "s-0") 'centaur-tabs-select-visible-tab)
  :config
  (setq centaur-tabs-height 25
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-style "bar"
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

(add-hook 'prog-mode-hook
          (lambda ()
            (push '("+=" . ?⩲) prettify-symbols-alist)
            (push '("*=" . ?⩮) prettify-symbols-alist)
            (push '("==" . ?≡) prettify-symbols-alist)
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)
            (push '("!=" . ?≠) prettify-symbols-alist)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (push '("defun"  . ?ƒ) prettify-symbols-alist)
            (push '("lambda" . ?λ) prettify-symbols-alist)
            (push '("/="     . ?≠) prettify-symbols-alist)
            (push '("sqrt"   . ?√) prettify-symbols-alist)
            (push '("not"    . ?¬) prettify-symbols-alist)
            (push '("and"    . ?∧) prettify-symbols-alist)
            (push '("or"     . ?∨) prettify-symbols-alist)))

(add-hook 'go-mode-hook
          (lambda ()
            (push '("->" . ?→) prettify-symbols-alist)
            (push '("<-" . ?←) prettify-symbols-alist)))

(mapc
 (lambda (hook)
   (add-hook hook (lambda ()
                    (push '("&&" . ?∧) prettify-symbols-alist)
                    (push '("||" . ?∨) prettify-symbols-alist)
                    (push '(">>" . ?») prettify-symbols-alist)
                    (push '("<<" . ?«) prettify-symbols-alist)
                    )))
 '(c-mode-hook c++-mode-hook))

(add-hook 'python-mode-hook
          (lambda ()
            (push '("def"     . ?ƒ) prettify-symbols-alist)
            (push '("sum"     . ?Σ) prettify-symbols-alist)
            (push '("**2"     . ?²) prettify-symbols-alist)
            (push '("**3"     . ?³) prettify-symbols-alist)
            (push '("None"    . ?∅) prettify-symbols-alist)
            (push '("in"      . ?∈) prettify-symbols-alist)
            (push '("not in"  . ?∉) prettify-symbols-alist)
            (push '("or"      . ?∨) prettify-symbols-alist)
            (push '("and"     . ?∧) prettify-symbols-alist)
            (push '("not"     . ?¬) prettify-symbols-alist)
            (push '("math.pi" . ?π) prettify-symbols-alist)))

(add-hook 'org-mode-hook
          (lambda ()
            (push '("[ ]"             . ?☐) prettify-symbols-alist)
            (push '("[X]"             . ?☑) prettify-symbols-alist)
            (push '("[-]"             . ?❍) prettify-symbols-alist)
            (push '("#+BEGIN_SRC"     . ?↦) prettify-symbols-alist)
            (push '("#+END_SRC"       . ?⇤) prettify-symbols-alist)
            (push '("#+BEGIN_EXAMPLE" . ?↦) prettify-symbols-alist)
            (push '("#+END_EXAMPLE"   . ?⇤) prettify-symbols-alist)
            (push '("#+BEGIN_QUOTE"   . ?↦) prettify-symbols-alist)
            (push '("#+END_QUOTE"     . ?⇤) prettify-symbols-alist)
            (push '("#+begin_quote"   . ?↦) prettify-symbols-alist)
            (push '("#+end_quote"     . ?⇤) prettify-symbols-alist)
            (push '("#+begin_example" . ?↦) prettify-symbols-alist)
            (push '("#+end_example"   . ?⇤) prettify-symbols-alist)
            (push '("#+begin_src"     . ?↦) prettify-symbols-alist)
            (push '("#+end_src"       . ?⇤) prettify-symbols-alist)))
;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode t)

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (emacs-lisp-mode . rainbow-mode))

(provide 'init-ui)
;;; init-ui ends here
