;;; go-dlv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "go-dlv" "go-dlv.el" (23169 3233 385771 414000))
;;; Generated autoloads from go-dlv.el

(autoload 'dlv "go-dlv" "\
Run dlv on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

\(fn COMMAND-LINE)" t nil)

(autoload 'dlv-current-func "go-dlv" "\
Debug the current program or test stopping at the beginning of the current function.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; go-dlv-autoloads.el ends here
