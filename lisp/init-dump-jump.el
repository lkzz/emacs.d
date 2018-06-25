;;; init-dump-jump.el --- dump jump
;;; Commentary:
;;; Code:


(use-package dumb-jump
  :defer t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :commands (hydra-dumb-jump/body)
  :init
  (progn
    (setq dumb-jump-prefer-searcher 'ag)
    (setq dumb-jump-selector 'ivy)
    (evil-leader/set-key "dj" #'hydra-dumb-jump/body)
    (defhydra hydra-dumb-jump (:color blue :columns 3)
      "Dumb Jump"
      ("j" dumb-jump-go "Go")
      ("o" dumb-jump-go-other-window "Other window")
      ("e" dumb-jump-go-prefer-external "Go external")
      ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
      ("i" dumb-jump-go-prompt "Prompt")
      ("l" dumb-jump-quick-look "Quick look")
      ("b" dumb-jump-back "Back"))
    ))

(provide 'init-dump-jump)
;;; init-dump-jump.el ends here
