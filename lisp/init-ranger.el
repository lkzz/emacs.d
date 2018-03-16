;;; init-ranger.el -- Initizlize ranger.
;;; Commentary:
;;; Code:

(use-package ranger
  :ensure t
  :demand t
  :commands ranger
  :init
  (setq ranger-override-dired t)
  :config
  (setq ranger-cleanup-on-disable t)
  (setq ranger-modify-header nil)
  ;; To exclude certain files (e.g. videos) from being previewed
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4" "pdf"))
  ;; Binary files will not be previewed, if this variable is set to t
  (setq ranger-dont-show-binary t)
  ;; Files that are larger than the max file size (in MB) variable, won't be previewed.
  (setq ranger-max-preview-size 20)
  ;; Show cursor in ranger
  (setq ranger-hide-cursor nil))

(provide 'init-ranger)
;;; init-ranger.el ends here
