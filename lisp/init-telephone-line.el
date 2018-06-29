;;; init-telephone-line.el --- modeline config for emacs.
;;; Commentary:
;;; Code:

(use-package telephone-line
  :config
  (progn
    (setq telephone-line-height 30)
    (setq telephone-line-lhs
          '((evil   . (telephone-line-evil-tag-segment))
            (accent . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment
                       telephone-line-process-segment))
            (nil    . (telephone-line-minor-mode-segment
                       telephone-line-buffer-segment))))
    (setq telephone-line-rhs
          '((nil    . (telephone-line-misc-info-segment))
            (accent . (telephone-line-major-mode-segment))
            (evil   . (telephone-line-airline-position-segment))))
    (telephone-line-evil-config)))

(provide 'init-telephone-line)
;;; init-telephone.el ends here
