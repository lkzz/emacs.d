;;; init-nlinum.el --- 配置行号
;;; Commentary:
;;; Code:

(setq-default nlinum-highlight-current-line t)
(setq-default nlinum-format "%3d ")

(add-hook 'prog-mode-hook 'nlinum-mode)
(add-hook 'text-mode-hook 'nlinum-mode)



(provide 'init-nlinum)
