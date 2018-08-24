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
  :init
  (setq-default python-indent-offset 4)
  (add-hook 'python-mode-hook (lambda ()
                                (setq tab-width 4)
                                (set-variable 'python-indent-offset 4)
                                (set-variable 'python-indent-guess-indent-offset nil)))
  :config
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (add-hook 'inferior-python-mode-hook (lambda ()
                                         (bind-key "C-c C-z"
                                                   'kill-buffer-and-window inferior-python-mode-map)
                                         (process-query-on-exit-flag (get-process "Python"))))
  )


;; Anaconda mode
(use-package anaconda-mode
  :ensure t
  :after python
  :diminish anaconda-mode
  :hook (python-mode . anaconda-mode))

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :init
  (add-hook 'python-mode-hook (lambda ()
                                (make-local-variable 'company-backends)
                                (setq company-backends (list 'company-anaconda 'company-dabbrev 'company-yasnippet))))
  )

;; Autopep8
(use-package py-autopep8
  :ensure t
  :after python
  :init (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

;; Anaconda mode
(use-package anaconda-mode
  :ensure t
  :defer t
  :after python
  :diminish anaconda-mode
  :init (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda
  :ensure t
  :after (python company anaconda-mode))


(provide 'init-python)
;;; init-python.el ends here
