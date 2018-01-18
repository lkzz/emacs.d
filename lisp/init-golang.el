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

;; (eval-after-load 'go-mode
;;   (require 'toml-mode))

;; ;; Ignore go test -c output files
;; (add-to-list 'completion-ignored-extensions ".test")

;; (define-key 'help-command (kbd "G") 'godoc)

;; (eval-after-load 'go-mode
;;   '(progn
;;      (defun kevin/go-mode-defaults ()
;;        ;; Add to default go-mode key bindings
;;        (let ((map go-mode-map))
;;          (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
;;          (define-key map (kbd "C-c m") 'go-test-current-file)
;;          (define-key map (kbd "C-c .") 'go-test-current-test)
;;          (define-key map (kbd "C-c b") 'go-run)
;;          (define-key map (kbd "C-c r") 'go-guru-referrers)
;;          (define-key map (kbd "C-h f") 'godoc-at-point))

;;        ;; Prefer goimports to gofmt if installed
;;        (let ((goimports (executable-find "goimports")))
;;          (when goimports
;;            (setq gofmt-command goimports)))

;;        ;; gofmt on save
;;        (add-hook 'before-save-hook 'gofmt-before-save nil t)

;;        ;; stop whitespace being highlighted
;;        (whitespace-toggle-options '(tabs))

;;        ;; Company mode settings
;;        (set (make-local-variable 'company-backends) '(company-go))

;;        ;; El-doc for Go
;;        (go-eldoc-setup)

;;        ;; CamelCase aware editing operations
;;        (subword-mode +1))

;;      (setq kevin/go-mode-hook 'kevin/go-mode-defaults)

;;      (add-hook 'go-mode-hook (lambda ()
;;                                (run-hooks 'kevin/go-mode-hook)))))

(use-package go-mode
  )

(provide 'init-golang)
;;; init-golang ends here
