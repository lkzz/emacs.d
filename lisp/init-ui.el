;;; init-ui.el ---  setup ui. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
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

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package doom-themes
  :custom-face
  ;; colors taken grom doom-gruvbox-theme.el
  (rainbow-delimiters-depth-1-face ((t (:foreground "#fb4934"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "#fabd2f"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "#8ec07c"))))
  (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
  :init
  (add-hook 'kevin-load-theme-hook #'doom-themes-org-config)
  (add-hook 'kevin-load-theme-hook #'doom-themes-neotree-config)
  (add-hook 'kevin-load-theme-hook #'doom-themes-visual-bell-config)
  (setq doom-dark+-blue-modeline t
        doom-gruvbox-dark-variant "medium"
        doom-themes-neotree-file-icons 't
        doom-themes-neotree-line-spacing 2)
  :config
  (setq rainbow-delimiters-max-face-count 3)
  ;; 加载主题
  (if (daemonp)
      (add-hook 'after-make-frame-functions (lambda (frame) (load-theme 'doom-gruvbox t)))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame (or frame (selected-frame))
                  (load-theme 'doom-gruvbox t))))
    (load-theme 'doom-gruvbox t)))

(when (display-graphic-p)
  ;; Frame maximized
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; Specify default font
  (cl-loop for font in '("JetBrains Mono" "Fira Code" "SF Mono" "Monaco")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height 150))
  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Color Emoji" "Symbola")
           when (font-installed-p font)
           return(set-fontset-font t 'unicode font nil 'prepend))
  ;; Specify font for Chinese characters
  (cl-loop for font in '("STKaiti" "WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

(use-package vi-tilde-fringe
  :if (fboundp 'set-fringe-mode)
  :diminish vi-tilde-fringe-mode
  :hook ((prog-mode text-mode conf-mode) . vi-tilde-fringe-mode))

;; config built-in "display-line-numbers-mode" (require Emacs >= 26)
(use-package display-line-numbers
  :straight (:type built-in)
  :hook ((prog-mode text-mode conf-mode protobuf-mode) . display-line-numbers-mode)
  :init
  (setq display-line-numbers-width 2
        display-line-numbers-widen t
        display-line-numbers-current-absolute t))

;; NOTE: Must run `M-x all-the-icons-install-fonts', and install fonts manually on Windows
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 0.95)
  (add-to-list 'all-the-icons-regexp-icon-alist
               '("^go.mod$" all-the-icons-fileicon "go" :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-regexp-icon-alist
               '("^go.sum$" all-the-icons-fileicon "go" :face all-the-icons-dpurple))
  (add-to-list 'all-the-icons-regexp-icon-alist
               '("\\.gitignore$" all-the-icons-alltheicon "git" :face all-the-icons-red))
  (add-to-list 'all-the-icons-regexp-icon-alist
               '("\\.gitmodules$" all-the-icons-alltheicon "git" :face all-the-icons-red)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (defun my-doom-modeline--font-height ()
    "Calculate the actual char height of the mode-line."
    (+ (frame-char-height) 2))
  (advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)
  (add-hook #'kevin-load-theme-hook #'doom-modeline-refresh-bars)
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  (setq doom-modeline-bar-width 3
        doom-modeline-env-version nil
        doom-modeline-env-enable-python t
        doom-modeline-icon (display-graphic-p)
        doom-modeline-minor-modes t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-modal-icon t
        doom-modeline-indent-info nil
        doom-modeline-unicode-fallback t
        doom-modeline-enable-word-count t
        doom-modeline-vcs-max-length 20
        doom-modeline-buffer-encoding t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-buffer-file-name-style 'auto))

(use-package minions
  :config (minions-mode 1))

(use-package hide-mode-line
  :hook ((neotree-mode . hide-mode-line-mode)
         (dashboard-mode . hide-mode-line-mode)
         (dired-mode . hide-mode-line-mode)))

;; Must install Fira Code font
(use-package ligature
  :if (display-graphic-p)
  :straight (ligature :host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable ligatures for nxml mode which doesn't derive from prog-mode:
  (ligature-set-ligatures 'nxml-mode '("<!--" "-->" "</"))
  ;; Enable all Fira Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't))

(use-package darkroom
  :general (my-space-leader-def "t d" '(kevin/toggle-darkroom-mode :wk "darkroom"))
  :init
  (setq darkroom-margins 0.15
        darkroom-text-scale-increase 0
        darkroom-fringes-outside-margins nil))

;; Display ugly ^L page breaks as tidy horizontal lines
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(use-package tree-sitter
  :straight (:host github :repo "emacsmirror/tree-sitter" :files (:defaults "*"))
  :if (bound-and-true-p module-file-suffix)
  :hook (prog-mode . tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :custom-face
  (tree-sitter-hl-face:property ((t (:inherit font-lock-constant-face))))
  :config
  (use-package tree-sitter-langs
    :straight (:host github :repo "emacsmirror/tree-sitter-langs" :files (:defaults "*")))
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  "Don't break with errors when current major mode lacks tree-sitter support."
  (advice-add 'tree-sitter-mode :around (lambda (orig-fn &rest args)
                                          (condition-case e
                                              (apply orig-fn args)
                                            (error
                                             (unless (string-match-p (concat "^Cannot find shared library\\|"
                                                                             "^No language registered\\|"
                                                                             "cannot open shared object file")
                                                                     (error-message-string e))
                                               (signal (car e) (cadr e))))))))

(provide 'init-ui)
;;; init-ui ends here
