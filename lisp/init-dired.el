;;; init-dired.el --- config dired mode
;;; Commentary:
;;; Code:
;; Show directory first

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

;; Directory operations
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh --group-directories-first") ; Show directory first
  (setq dired-dwim-target t)            ; copy in a split window
  (setq dired-recursive-deletes 'top)   ; "top" means ask cone
  (setq dired-recursive-copies 'always);; "always" means no asking
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

  (cond
   (sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    ;; Use GNU ls as `gls' from `coreutils' if available.
    (when (executable-find "gls")
      (setq insert-directory-program "gls")))
   (sys/win32p
    (when (executable-find "ls")
      ;; `dired-quick-sort' needs it
      (setq ls-lisp-use-insert-directory-program t))))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :demand
    :after dired
    :config
    (when (display-graphic-p)
      (setq dired-guess-shell-alist-user
            '(("\\.pdf\\'" "open")
              ("\\.docx\\'" "open")
              ("\\.\\(?:djvu\\|eps\\)\\'" "open")
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
              ("\\.\\(?:xcf\\)\\'" "open")
              ("\\.csv\\'" "open")
              ("\\.tex\\'" "open")
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"
               "open")
              ("\\.\\(?:mp3\\|flac\\)\\'" "open")
              ("\\.html?\\'" "open")
              ("\\.md\\'" "open"))))
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

  ;; Quick sort dired buffers via hydra
  ;; bind key: `S'
  (use-package dired-quick-sort
    :if (or (executable-find "gls") (executable-find "ls"))
    :init (dired-quick-sort-setup))

  ;; Extended file highlighting according to its type
  (use-package dired-rainbow
    :commands dired-rainbow-define dired-rainbow-define-chmod
    :init
    (dired-rainbow-define dotfiles "gray" "\\..*")

    (dired-rainbow-define web "#4e9a06" ("htm" "html" "xhtml" "xml" "xaml" "css" "js"
                                         "json" "asp" "aspx" "haml" "php" "jsp" "ts"
                                         "coffee" "scss" "less" "phtml"))
    (dired-rainbow-define prog "yellow3" ("el" "l" "ml" "py" "rb" "pl" "pm" "c"
                                          "cpp" "cxx" "c++" "h" "hpp" "hxx" "h++"
                                          "m" "cs" "mk" "make" "swift" "go" "java"
                                          "asm" "robot" "yml" "yaml" "rake" "lua"))
    (dired-rainbow-define sh "green yellow" ("sh" "bash" "zsh" "fish" "csh" "ksh"
                                             "awk" "ps1" "psm1" "psd1" "bat" "cmd"))
    (dired-rainbow-define text "yellow green" ("txt" "md" "org" "ini" "conf" "rc"
                                               "vim" "vimrc" "exrc"))
    (dired-rainbow-define doc "spring green" ("doc" "docx" "ppt" "pptx" "xls" "xlsx"
                                              "csv" "rtf" "wps" "pdf" "texi" "tex"
                                              "odt" "ott" "odp" "otp" "ods" "ots"
                                              "odg" "otg"))
    (dired-rainbow-define misc "gray50" ("DS_Store" "projectile" "cache" "elc"
                                         "dat" "meta"))
    (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "wav" "wma"
                                           "wmv" "mov" "3gp" "avi" "mpg" "mkv"
                                           "flv" "ogg" "rm" "rmvb"))
    (dired-rainbow-define picture "purple3" ("bmp" "jpg" "jpeg" "gif" "png" "tiff"
                                             "ico" "svg" "psd" "pcd" "raw" "exif"
                                             "BMP" "JPG" "PNG"))
    (dired-rainbow-define archive "saddle brown" ("zip" "tar" "gz" "tgz" "7z" "rar"
                                                  "gzip" "xz" "001" "ace" "bz2" "lz"
                                                  "lzma" "bzip2" "cab" "jar" "iso"))

    ;; boring regexp due to lack of imagination
    (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")

    ;; highlight executable files, but not directories
    (dired-rainbow-define-chmod executable-unix "green" "-[rw-]+x.*"))

  ;; Highlights dired buffer like k
  (use-package dired-k
    :bind (:map dired-mode-map ("K" . dired-k))
    :init
    (setq dired-k-padding 1)
    (setq dired-k-human-readable t)))

(provide 'init-dired)
;;; init-dired ends here
