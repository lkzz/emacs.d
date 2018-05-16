;; init-golang.el --- Initialize Golang configurations.
;;; Commentary:
;;; Code:

;; Go packages:
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/dougm/goflymake

;; FIXME: `go-guru' doesn't work on Windows. Use `godef' instead.
;; https://github.com/dominikh/go-mode.el/issues/218

(use-package go-mode
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
               "go build -v")))
    (add-hook 'go-mode-hook 'setup-go-mode-compile)
    (add-hook 'before-save-hook #'gofmt-before-save)
    (add-hook 'after-save-hook #'kevin/revert-buffer-no-confirm)))

(use-package golint
  :after go-mode)

(use-package govet
  :after go-mode)

(use-package go-eldoc
  :after (go-mode eldoc)
  :config
  (progn (add-hook 'go-mode-hook 'go-eldoc-setup)))

(use-package go-errcheck
  :after go-mode)

(use-package go-guru
  :after go-mode
  :commands (go-guru-describe go-guru-freevars go-guru-implements go-guru-peers
                              go-guru-referrers go-guru-definition go-guru-pointsto
                              go-guru-callstack go-guru-whicherrs go-guru-callers go-guru-callees
                              go-guru-expand-region)
  :bind (:map go-mode-map
              ("C-c d" . go-guru-definition)
              ("C-c r" . go-guru-referrers)))

(use-package gotest
  :after go-mode
  :bind (:map go-mode-map
              ("C-c a" . go-test-current-project)
              ("C-c m" . go-test-current-file)
              ("C-c ." . go-test-current-test)
              ("C-c x" . go-run)))

(use-package company-go
  :after (go-mode company)
  :config
  (progn
    (add-to-list 'company-backends 'company-go)
    ))

;; (progn (add-hook 'go-mode-hook (lambda ()
;;                                  (set (make-local-variable 'company-backends) '(company-go))
;;                                  (company-mode)))))

;; (use-package flycheck-gometalinter
;;   :ensure t
;;   :after (go-mode flycheck-mode)
;;   :config
;;   (progn
;;     (setq flycheck-gometalinter-fast t)
;;     ;; only show errors
;;     ;; (setq flycheck-gometalinter-errors-only t)
;;     ;; skips 'vendor' directories
;;     (setq flycheck-gometalinter-vendor t)
;;     (setq flycheck-gometalinter-deadline "15s")
;;     (setq flycheck-gometalinter-concurrency 1)
;;     (setq flycheck-gometalinter-disable-all t)
;;     (setq flycheck-gometalinter-enable-linters '("golint" "errcheck" "vet" "deadcode" "staticcheck"))
;;     (defun kevin/configure-metalinter ()
;;       "Enable `flycheck-gometalinter' and disable overlapping `flycheck' linters."
;;       (setq flycheck-disabled-checkers '(go-gofmt
;;                                          go-golint
;;                                          go-vet
;;                                          go-build
;;                                          go-test
;;                                          go-errcheck))
;;       (flycheck-gometalinter-setup))
;;     (add-hook 'go-mode-hook 'kevin/go-enable-gometalinter t)))


(provide 'init-golang)
;;; init-golang ends here
