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
              ("<f1>" . godoc-at-point))
  :config
  ;; `goimports' or `gofmt'
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)

  (use-package go-dlv
    :defer t
    :ensure t)
  (use-package go-fill-struct
    :defer t
    :ensure t)
  (use-package go-impl
    :defer t
    :ensure t)
  (use-package go-playground
    :defer t
    :ensure t)
  (use-package golint
    :defer t
    :ensure t)
  (use-package govet
    :defer t
    :ensure t)

  (use-package go-eldoc
    :defer t
    :ensure t
    :diminish eldoc-mode
    :init (add-hook 'go-mode-hook #'go-eldoc-setup))

  (use-package go-guru
    :defer t
    :ensure t
    :bind (:map go-mode-map
                ("C-c d" . go-guru-definition)
                ("C-c r" . go-guru-referrers)))

  (use-package go-tag
    :defer t
    :ensure t
    :bind (:map go-mode-map
                ("C-c t" . go-tag-add)
                ("C-c T" . go-tag-remove)))

  (use-package gotest
    :defer t
    :ensure t
    :bind (:map go-mode-map
                ("C-c a" . go-test-current-project)
                ("C-c m" . go-test-current-file)
                ("C-c ." . go-test-current-test)
                ("C-c x" . go-run)))

  (use-package go-gen-test
    :defer t
    :ensure t
    :bind (:map go-mode-map
                ("C-c C-g" . go-gen-test-dwim)))

  (with-eval-after-load 'company
    (use-package company-go
      :ensure t
      :defer t
      :init (cl-pushnew (company-backend-with-yas 'company-go) company-backends)))

  (with-eval-after-load 'projectile
    ;; M-x `go-projectile-install-tools'
    (use-package go-projectile
      :ensure t
      :defer t
      :commands (go-projectile-mode go-projectile-switch-project)
      :init
      (add-hook 'projectile-after-switch-project-hook #'go-projectile-switch-project)
      (add-hook 'go-mode-hook #'go-projectile-mode))))

(use-package flycheck-gometalinter
  :ensure t
  :after go-mode
  :init
  (setq flycheck-gometalinter-fast t)
  ;; only show errors
  ;; (setq flycheck-gometalinter-errors-only t)
  ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  (setq flycheck-gometalinter-vendor t)
  (setq flycheck-gometalinter-deadline "15s")
  (setq flycheck-gometalinter-concurrency 3)
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
  (add-hook 'go-mode-hook 'kevin/go-enable-gometalinter t))

(provide 'init-golang)
;;; init-golang ends here
