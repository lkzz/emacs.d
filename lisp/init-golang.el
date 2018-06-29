;; init-golang.el --- Initialize Golang configurations.
;;; Commentary:
;;; Code:

;; Go packages:
;; go get -u github.com/nsf/gocode
;; go get github.com/sourcegraph/go-langserver
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/dougm/goflymake
;; go get github.com/godoctor/godoctor

;; FIXME: `go-guru' doesn't work on Windows. Use `godef' instead.
;; https://github.com/dominikh/go-mode.el/issues/218

(use-package go-mode
  :defer t
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
    (add-hook 'after-save-hook #'kevin/revert-buffer-no-confirm)
    (add-hook 'before-save-hook 'gofmt-before-save)

    (kevin/declare-prefix-for-mode 'go-mode "me" "playground")
    (kevin/declare-prefix-for-mode 'go-mode "mg" "goto")
    (kevin/declare-prefix-for-mode 'go-mode "mh" "help")
    (kevin/declare-prefix-for-mode 'go-mode "mi" "imports")
    (kevin/declare-prefix-for-mode 'go-mode "mt" "test")
    (kevin/declare-prefix-for-mode 'go-mode "mx" "execute")
    (kevin/set-leader-keys-for-major-mode 'go-mode
      "cj" 'godef-jump
      "hh" 'godoc-at-point
      "ig" 'go-goto-imports
      "ia" 'go-import-add
      "ir" 'go-remove-unused-imports
      "eb" 'go-play-buffer
      "er" 'go-play-region
      "ed" 'go-download-play
      "xx" 'go-run
      "ga" 'ff-find-other-file
      "gc" 'go-coverage
      "tt" 'go-test-current-test
      "tm" 'go-test-current-file
      "tp" 'go-test-current-project)))

(use-package golint
  :after go-mode)

(use-package govet
  :after go-mode)

(use-package go-eldoc
  :after (go-mode eldoc)
  :commands (godoc-at-point)
  :hook (go-mode . go-eldoc-setup))

(use-package go-errcheck
  :after go-mode)

(use-package go-guru
  :after go-mode
  :commands (go-guru-describe go-guru-freevars go-guru-implements go-guru-peers
                              go-guru-referrers go-guru-definition go-guru-pointsto
                              go-guru-callstack go-guru-whicherrs go-guru-callers go-guru-callees
                              go-guru-set-scope)
  :init
  (progn
    (kevin/declare-prefix-for-mode 'go-mode "mf" "guru")
    (kevin/set-leader-keys-for-major-mode 'go-mode
      "fd" 'go-guru-describe
      "ff" 'go-guru-freevars
      "fi" 'go-guru-implements
      "fc" 'go-guru-peers
      "fr" 'go-guru-referrers
      "fj" 'go-guru-definition
      "fp" 'go-guru-pointsto
      "fs" 'go-guru-callstack
      "fe" 'go-guru-whicherrs
      "f<" 'go-guru-callers
      "f>" 'go-guru-callees
      "fo" 'go-guru-set-scope)))

(use-package gotest
  :after go-mode
  :commands (go-test-current-project go-test-current-file go-test-current-test))

(use-package go-rename
  :init
  (progn
    (kevin/declare-prefix-for-mode 'go-mode "mr" "refactoring")
    (kevin/set-leader-keys-for-major-mode 'go-mode "rN" 'go-rename)))

(use-package godoctor
  :defer t
  :init
  (progn
    (kevin/declare-prefix-for-mode 'go-mode "mr" "refactoring")
    (kevin/set-leader-keys-for-major-mode 'go-mode
      "rn" 'godoctor-rename
      "re" 'godoctor-extract
      "rt" 'godoctor-toggle
      "rd" 'godoctor-godoc)))

(use-package go-tag
  :init
  (kevin/declare-prefix-for-mode 'go-mode "mr" "refactoring")
  (kevin/set-leader-keys-for-major-mode 'go-mode
    "rf" 'go-tag-add
    "rF" 'go-tag-remove))

(defun kevin/setup-go-backends ()
  (let ((local-go-backends kevin/company-global-backends))
    (add-to-list 'local-go-backends 'company-go)
    (set (make-local-variable 'company-backends) local-go-backends)))

(use-package company-go
  :after (go-mode company)
  :hook (go-mode . kevin/setup-go-backends))


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
