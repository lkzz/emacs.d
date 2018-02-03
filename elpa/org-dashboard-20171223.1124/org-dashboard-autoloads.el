;;; org-dashboard-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "org-dashboard" "org-dashboard.el" (23157 33501
;;;;;;  901372 919000))
;;; Generated autoloads from org-dashboard.el

(autoload 'org-dashboard-display "org-dashboard" "\


\(fn)" t nil)

(autoload 'org-dblock-write:block-dashboard "org-dashboard" "\
Generate a progress report inside an org dynamic block.

Progress information is retrieved by searching files in
`org-dashboard-files' for headings containing a \"progress cookie\",
e.g.:

  ** [50%] release v0.1
  *** TODO publish on github
  *** DONE import in git

See Info node `(org) Breaking down tasks'.

\(fn PARAMS)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-dashboard-autoloads.el ends here
