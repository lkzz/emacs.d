;;; init-python.el --- Initialize python configurations. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2019  Kevin Leung
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

;; ;; Python Mode
;; (use-package python
;;   :ensure nil
;;   :init
;;   (setq-default python-indent-offset 4)
;;   (add-hook 'python-mode-hook (lambda ()
;;                                 (setq tab-width 4)
;;                                 (set-variable 'python-indent-offset 4)
;;                                 (set-variable 'python-indent-guess-indent-offset nil)))
;;   :config
;;   ;; Disable readline based native completion
;;   (setq python-shell-completion-native-enable nil)
;;   (add-hook 'inferior-python-mode-hook (lambda ()
;;                                          (bind-key "C-c C-z"
;;                                                    'kill-buffer-and-window inferior-python-mode-map)
;;                                          (process-query-on-exit-flag (get-process "Python"))))
;;   )


;; ;; Anaconda mode
;; (use-package anaconda-mode
;;   :after python
;;   :diminish anaconda-mode
;;   :hook (python-mode . anaconda-mode))

;; (use-package company-anaconda
;;   :after (company anaconda-mode)
;;   :init
;;   (add-hook 'python-mode-hook (lambda ()
;;                                 (make-local-variable 'company-backends)
;;                                 (setq company-backends (list 'company-anaconda 'company-dabbrev 'company-yasnippet))))
;;   )

;; ;; Autopep8
;; (use-package py-autopep8
;;   :after python
;;   :init (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

;; ;; Anaconda mode
;; (use-package anaconda-mode
;;   :after python
;;   :diminish anaconda-mode
;;   :init (add-hook 'python-mode-hook #'anaconda-mode))

;; (use-package company-anaconda
;;   :after (python company anaconda-mode))

;; Python Mode
;; Install:
;;   pip install pyflakes
;;   pip install autopep8
(use-package python
  :ensure nil
  :defines gud-pdb-command-name pdb-path
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; (bind-key "C-c C-z" #'kill-buffer-and-window inferior-python-mode-map)
              (process-query-on-exit-flag (get-process "Python"))))

  ;; Pdb setup, note the python version
  (setq pdb-path 'pdb
        gud-pdb-command-name (symbol-name pdb-path))
  (defadvice pdb (before gud-query-cmdline activate)
    "Provide a better default command line when called interactively."
    (interactive
     (list (gud-query-cmdline
            pdb-path
            (file-name-nondirectory buffer-file-name)))))

  ;; Live Coding in Python
  (use-package live-py-mode)

  ;; Autopep8
  (use-package py-autopep8
    :hook (python-mode . py-autopep8-enable-on-save))

  ;; Anaconda mode
  (unless kevin-lsp-mode-enable-p
    (use-package anaconda-mode
      :defines anaconda-mode-localhost-address
      :diminish anaconda-mode
      :hook ((python-mode . anaconda-mode)
             (python-mode . anaconda-eldoc-mode))
      :config
      ;; WORKAROUND: https://github.com/proofit404/anaconda-mode#faq
      (when kevin-mac-p
        (setq anaconda-mode-localhost-address "localhost"))
      (use-package company-anaconda
        :after company
        :defines company-backends
        :functions company-backend-with-yas
        :init (cl-pushnew (company-backend-with-yas 'company-anaconda) company-backends)))))


(provide 'init-python)
;;; init-python.el ends here
