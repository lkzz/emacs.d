;;; govet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "govet" "govet.el" (23169 3235 866503 653000))
;;; Generated autoloads from govet.el

(autoload 'govet "govet" "\
Run govet on the current file and populate the fix list.
Pressing \\[next-error] will jump directly to the line in your
code which caused the first message.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; govet-autoloads.el ends here
