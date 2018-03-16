;;; init-ggtags.el --- Initialize ggtags configurations.
;;; Commentary:
;;; Code:

(use-package ggtags
  :ensure t
  :defer t
  :commands ggtags-mode
  :diminish ggtags-mode)

(provide 'init-ggtags)
;;; init-ggtags.el ends here
