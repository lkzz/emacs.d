;;; init-ranger.el -- Initizlize ranger.
;;; Commentary:
;;; Code:

(use-package ranger
  :demand t
  :commands (ranger deer deer-jump-other-window ranger-override-dired-mode)
  :init
  (progn
    (setq ranger-override-dired t)
    (kevin/set-leader-keys
      "jd" 'deer
      "jr" 'ranger)
    (eval-after-load 'evil
      '(progn
         (evil-define-key 'normal ranger-mode-map (kbd "q") 'ranger-close)))
    )
  :config
  (setq ranger-cleanup-on-disable t)
  (setq ranger-modify-header nil)
  ;; To exclude certain files (e.g. videos) from being previewed
  (setq ranger-excluded-extensions '("mkv" "iso" "mp4" "pdf"))
  ;; Binary files will not be previewed, if this variable is set to t
  (setq ranger-dont-show-binary t)
  (setq ranger-preview-file t)
  ;; Files that are larger than the max file size (in MB) variable, won't be previewed.
  (setq ranger-max-preview-size 20)
  ;; Hide dotfiles
  (setq ranger-show-hidden nil)
  (setq ranger-parent-depth 0)
  (setq ranger-max-parent-width 0.12)
  ;; Show cursor in ranger
  (setq ranger-hide-cursor nil))

(provide 'init-ranger)
;;; init-ranger.el ends here
