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

;; FIXME: `go-guru' doesn't work on Windows. Use `godef' instead.
;; https://github.com/dominikh/go-mode.el/issues/218

(use-package go-mode
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("C-c C-r" . go-remove-unused-imports)
              ("<f1>" . godoc-at-point))
  :config
  ;; `goimports' or `gofmt'
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)

  (use-package go-dlv)
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-playground)
  (use-package golint)
  (use-package govet)

  (use-package go-eldoc
    :init (add-hook 'go-mode-hook #'go-eldoc-setup))

  (use-package go-guru
    :bind (:map go-mode-map
                ("C-c d" . go-guru-definition)
                ("C-c r" . go-guru-referrers)))

  (use-package go-tag
    :bind (:map go-mode-map
                ("C-c t" . go-tag-add)
                ("C-c T" . go-tag-remove)))

  (use-package gotest
    :bind (:map go-mode-map
                ("C-c a" . go-test-current-project)
                ("C-c m" . go-test-current-file)
                ("C-c ." . go-test-current-test)
                ("C-c x" . go-run)))

  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c C-g" . go-gen-test-dwim)))

  (with-eval-after-load 'company
    (use-package company-go
      :init (cl-pushnew (company-backend-with-yas 'company-go) company-backends)))

  (with-eval-after-load 'projectile
    ;; M-x `go-projectile-install-tools'
    (use-package go-projectile
      :commands (go-projectile-mode go-projectile-switch-project)
      :init
      (add-hook 'projectile-after-switch-project-hook #'go-projectile-switch-project)
      (add-hook 'go-mode-hook #'go-projectile-mode))))


(provide 'init-golang)
;;; init-golang ends here
