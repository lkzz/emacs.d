;;; init-windows-popup.el ---  popup windows config.-*- lexical-binding: t -*-
;;; Commentary:
;;      https://github.com/wasamasa/shackle
;;; Author: kevin <kevin.scnu@gmail.com>
;;; URL: https://github.com/lkzz/emacs.d
;;; Code:

;; Popup Window Manager
(use-package popwin
  :defer t
  :ensure t
  :commands popwin-mode
  :init (add-hook 'after-init-hook #'popwin-mode)
  :config
  ;; don't use default value but manage it ourselves
  (setq popwin:special-display-config
        '(
          ("*Help*" :dedicated t :position bottom :stick t :noselect nil)
          ("*compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Compile-Log*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          ("*Warnings*" :dedicated t :position bottom :stick t :noselect t)
          ("*Completions*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*Async Shell Command\*.+" :regexp t :position bottom :stick t :noselect nil)
          ("^*Man.+*$" :regexp t :position bottom :stick nil :noselect nil :height 0.4)
          ("^*WoMan.+*$" :regexp t :position bottom)
          ("^*Backtrace.+*$" :regexp t :dedicated t :position bottom :stick t :noselect nil)
          ;; Kill Ring
          ("*Kill Ring*" :dedicated t :position bottom)
          ;; Flycheck
          ("\*Flycheck errors\*.+*$" :regexp t :position bottom :stick t :noselect nil)
          ;; Youdao dict
          ("*Youdao Dictionary*" :dedicated t :position bottom)
          ;; Paradox
          ("*Paradox Report*" :dedicated t :position bottom :noselect nil)
          ;; List
          ("*Colors*" :dedicated t :position bottom)
          ("*Process List*" :dedicated t :position bottom)
          ("*Process-Environment*" :dedicated t :position bottom)
          ;; undo-tree
          (" *undo-tree*" :dedicated t :position right :stick t :noselect nil :width 60)
          ;; Search
          ("*grep*" :dedicated t :position bottom :stick t :noselect nil)
          ("*ag search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*rg*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*pt-search*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          ("*Occur*" :dedicated t :position bottom :stick t :noselect nil)
          ("\*ivy-occur.+*$" :regexp t :position bottom :stick t :noselect nil)
          ("*xref*" :dedicated t :position bottom :stick nil :noselect nil)
          ;; VC
          ("*vc-diff*" :dedicated t :position bottom :stick t :noselect nil)
          ("*vc-change-log*" :dedicated t :position bottom :stick t :noselect nil)
          ;; Magit
          (magit-status-mode :dedicated t :position bottom :stick t :height 0.5)
          (magit-diff-mode :dedicated t :position bottom :stick t :noselect t :height 0.5)
          ;; Script
          ("*shell*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Python*" :dedicated t :position bottom :stick t :noselect t)
          ("*Ruby*" :dedicated t :position bottom :stick t :noselect t)
          ("*quickrun*" :dedicated t :position bottom :stick t :noselect t)
          ;; Go
          ("^*godoc.+*$" :regexp t :position bottom :stick nil :noselect nil)
          ("*golint*" :dedicated t :position bottom :stick t :noselect nil)
          ("*govet*" :dedicated t :position bottom :stick t :noselect nil)
          ("*go-guru-output*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Gofmt Errors*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Go Test*" :dedicated t :position bottom :stick t :noselect nil)
          ;; Test
          ("*ert*" :dedicated t :position bottom :stick t :noselect nil)
          ("*Ibuffer*" :dedicated t :position bottom :stick t :noselect nil)
          ("*nosetests*" :dedicated t :position bottom :stick t :noselect nil))))

;; (use-package shackle
;;   :ensure t
;;   :config
;;   (setq shackle-lighter "")
;;   (setq shackle-select-reused-windows nil) ; default nil
;;   (setq shackle-default-alignment 'below) ; default below
;;   (setq shackle-default-size 0.4) ; default 0.5
;;   (setq shackle-default-rule '(:select t))
;;   (setq shackle-rules
;;         '((compilation-mode :select nil )
;;           ("*undo-tree*" :size 0.25 :align right)
;;           ("*eshell*" :select t :other t )
;;           ("*Shell Command Output*" :select nil )
;;           ("\\*Async Shell.*\\*" :regexp t :ignore t )
;;           (occur-mode :select nil :align t )
;;           ("*Help*" :select t :inhibit-window-quit t)
;;           ("*Completions*" :size 0.3 :align t )
;;           ("*Messages*" :select nil :inhibit-window-quit t :other t )
;;           ("\\*[Wo]*Man.*\\*" :regexp t :select t :inhibit-window-quit t :other t )
;;           ("\\*poporg.*\\*" :regexp t :select t :other t )
;;           ("*Calendar*" :select t :size 0.3 :align below)
;;           ("*info*" :select t :inhibit-window-quit t :same t)
;;           (magit-status-mode :select t :inhibit-window-quit t :same t)
;;           (magit-log-mode :select t :inhibit-window-quit t :same t)))
;;   (shackle-mode 1)
;;   )

(provide 'init-windows-popup)
;;; init-windows-popup ends here
