;;; init-helm.el --- helm config. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x C-d" . helm-browse-project)
         ("C-x c o" . helm-occur)
         ("C-h a" . helm-apropos)
         ("M-y" . helm-show-kill-ring))
  :hook (after-init . helm-mode)
  :config
  (setq helm-candidate-number-limit 100
        helm-idle-delay 0.01
        helm-input-idle-delay 0.01
        helm-yas-display-key-on-candidate t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-split-window-in-side-p t
        helm-swoop-split-with-multiple-windows t)
  (kevin/set-leader-keys "SPC" 'helm-M-x
                         "ff" 'helm-find-files
                         "fr" 'helm-recentf
                         "bb" 'helm-mini)
  )

(use-package helm-swoop
  :ensure t
  :after helm
  :commands helm-swoop
  :bind ("C-s" . helm-swoop)
  :config
  (kevin/declare-prefix "s" "search")
  (kevin/set-leader-keys "ss" 'helm-swoop))

(use-package helm-rg
  :ensure t
  :after helm
  :config (kevin/set-leader-keys "/" 'helm-rg))

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (kevin/set-leader-keys "pp" 'helm-projectile-switch-project
                         "pf" 'helm-projectile-find-file
                         "pF" 'helm-projectile-find-file-dwim
                         "pd" 'helm-projectile-find-dir
                         "pb" 'helm-projectile-switch-to-buffer
                         "p/" 'helm-projectile-rg))

;; Better Fuzzy Matching for emacs Helm
(use-package helm-fuzzier
  :ensure t
  :after helm
  :init
  (helm-fuzzier-mode 1))


;; Flx-based fuzzy sorting for helm
(use-package helm-flx
  :ensure t
  :after helm
  :init
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t)
  (setq helm-flx-for-helm-locate t))

;; Emacs theme selection with helm interface
(use-package helm-themes
  :ensure t
  :after helm
  :init
  (require 'helm-themes))

(provide 'init-helm)
;;; init-helm ends here.
