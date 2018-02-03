;;; mwim-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "mwim" "mwim.el" (23157 33491 968045 159000))
;;; Generated autoloads from mwim.el
 (autoload 'mwim-beginning-of-line-or-code "mwim" nil t)
 (autoload 'mwim-beginning-of-code-or-line "mwim" nil t)
 (autoload 'mwim-beginning-of-code-or-line-or-comment "mwim" nil t)
 (autoload 'mwim-end-of-line-or-code "mwim" nil t)
 (autoload 'mwim-end-of-code-or-line "mwim" nil t)

(autoload 'mwim-beginning "mwim" "\
Move point to the next beginning position
Available positions are defined by `mwim-beginning-position-functions'.
See `mwim-move-to-next-position' for details.
Interactively, with prefix argument, move to the previous position.

\(fn &optional ARG)" t nil)

(autoload 'mwim-end "mwim" "\
Move point to the next end position.
Available positions are defined by `mwim-end-position-functions'.
See `mwim-move-to-next-position' for details.
Interactively, with prefix argument, move to the previous position.

\(fn &optional ARG)" t nil)

(autoload 'mwim "mwim" "\
Switch between various positions on the current line.
Available positions are defined by `mwim-position-functions'
variable.
Interactively, with prefix argument, move to the previous position.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mwim-autoloads.el ends here
