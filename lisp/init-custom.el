;;; init-custom.el --- Initialize custom configurations.
;;; Commentary:
;;; Code:


;; -----------------------------------------------------------------------------
;; const variables
;; -----------------------------------------------------------------------------
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defvar kevin/cache-directory (expand-file-name "cache/" user-emacs-directory)
  "Emacs cache directory.")

(defcustom my-theme 'default
  "Set color theme."
  :type '(choice
          (const :tag "Monokai theme" default)
          (const :tag "Tomorrow night theme" dark)
          (const :tag "Leuven light theme" light)
          (const :tag "Doom theme" doom)))

(setq my-theme 'doom)

(provide 'init-custom)
;;; init-custom.el ends here
