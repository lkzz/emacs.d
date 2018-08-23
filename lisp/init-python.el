;;; init-python.el --- Initialize python configurations. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;; Code:

;; Python Mode
(use-package python
  :defer t
  :ensure nil
  :defines gud-pdb-command-name pdb-path
  :init
  (setq-default python-indent-offset 4)
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (bind-key "C-c C-z"
                        'kill-buffer-and-window inferior-python-mode-map)
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

  ;; Autopep8
  (use-package py-autopep8
    :defer t
    :init (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

  ;; Anaconda mode
  (use-package anaconda-mode
    :defer t
    :diminish anaconda-mode
    :init (add-hook 'python-mode-hook #'anaconda-mode)
    :config
    (with-eval-after-load 'company
      (use-package company-anaconda
        :defer t
        ;; :defines company-backends
        ;; :init (cl-pushnew '(company-backend-with-yas 'company-anaconda) company-backends)

        ))))

(provide 'init-python)
;;; init-python.el ends here
