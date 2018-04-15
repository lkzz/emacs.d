;; init-golang.el --- Initialize Golang configurations.
;;; Commentary:
;;; Code:

;; Go packages:
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/derekparker/delve/cmd/dlv
;; go get -u github.com/josharian/impl
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
;; go get -u github.com/dougm/goflymake

;; FIXME: `go-guru' doesn't work on Windows. Use `godef' instead.
;; https://github.com/dominikh/go-mode.el/issues/218

(use-package go-mode
  :ensure t
  :defer t
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c C-d" . godoc-at-point))
  :config
  (progn
    (setq gofmt-command "goimports") ; use goimports instead of go-fmt
    (setq godoc-command "godoc")     ; use godoc instead of go doc
    (defun setup-go-mode-compile ()
      "Customize compile command to run go build"
      (if (not (string-match "go" compile-command))
          (set (make-local-variable 'compile-command)
               "go build -v && go test -v && go vet")))
    (add-hook 'go-mode-hook 'setup-go-mode-compile)
    (add-hook 'before-save-hook #'gofmt-before-save)))

;; (use-package golint
;;   :ensure t
;;   :after go-mode)

;; (use-package govet
;;   :ensure t
;;   :after go-mode)

(use-package go-eldoc
  :ensure t
  :after (go-mode eldoc)
  :config
  (progn (add-hook 'go-mode-hook 'go-eldoc-setup)))

;; (use-package go-errcheck
;;   :ensure t
;;   :after go-mode)

;; (use-package go-stacktrace
;;   :ensure t)

(use-package go-guru
  :ensure t
  :after go-mode
  :bind (:map go-mode-map
              ("C-c d" . go-guru-definition)
              ("C-c r" . go-guru-referrers)))

(use-package gotest
  :ensure t
  :after go-mode
  :bind (:map go-mode-map
              ("C-c a" . go-test-current-project)
              ("C-c m" . go-test-current-file)
              ("C-c ." . go-test-current-test)
              ("C-c x" . go-run)))

(use-package company-go
  :ensure t
  :after (go-mode company)
  :config
  (progn (add-hook 'go-mode-hook (lambda ()
                                   (set (make-local-variable 'company-backends) '(company-go))
                                   (company-mode)))))
;; M-x `go-projectile-install-tools'
(use-package go-projectile
  :ensure t
  :after (go-mode projectile-mode)
  :commands (go-projectile-mode go-projectile-switch-project)
  :init
  (add-hook 'projectile-after-switch-project-hook #'go-projectile-switch-project)
  (add-hook 'go-mode-hook #'go-projectile-mode))

(use-package flycheck-gometalinter
  :ensure t
  :after (go-mode flycheck-mode)
  :config
  (progn
    (setq flycheck-gometalinter-fast t)
    ;; only show errors
    ;; (setq flycheck-gometalinter-errors-only t)
    ;; skips 'vendor' directories
    (setq flycheck-gometalinter-vendor t)
    (setq flycheck-gometalinter-deadline "15s")
    (setq flycheck-gometalinter-concurrency 1)
    (setq flycheck-gometalinter-disable-all t)
    (setq flycheck-gometalinter-enable-linters '("golint" "errcheck" "vet" "deadcode" "staticcheck"))
    (defun kevin/configure-metalinter ()
      "Enable `flycheck-gometalinter' and disable overlapping `flycheck' linters."
      (setq flycheck-disabled-checkers '(go-gofmt
                                         go-golint
                                         go-vet
                                         go-build
                                         go-test
                                         go-errcheck))
      (flycheck-gometalinter-setup))
    (add-hook 'go-mode-hook 'kevin/go-enable-gometalinter t)))


(provide 'init-golang)
;;; init-golang ends here
