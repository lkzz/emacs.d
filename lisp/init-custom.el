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

(defvar kevin/leader-key "SPC"
  "The leader key.")

(defvar kevin/emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar kevin/major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar kevin/major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar kevin/default-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defvar kevin/cache-directory (expand-file-name "cache/" user-emacs-directory)
  "Emacs cache directory.")

(defvar kevin/default-directory "~/Code/gopath/src/go-common/app"
  "Default project directory.")

(defvar kevin/user-name "kevin leung"
  "Default username.")

(defvar kevin/mail-address "kevin.scnu@gmail.com"
  "Default email address.")

(defcustom my-theme 'default
  "Set color theme."
  :type '(choice
          (const :tag "zenburn theme" default)
          (const :tag "doom theme" dark)
          (const :tag "Leuven light theme" light)))

(setq my-theme 'dark)

(provide 'init-custom)
;;; init-custom.el ends here
