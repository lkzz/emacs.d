;;; highlight-indent-guides-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "highlight-indent-guides" "highlight-indent-guides.el"
;;;;;;  (23168 11252 721567 624000))
;;; Generated autoloads from highlight-indent-guides.el

(autoload 'highlight-indent-guides-auto-set-faces "highlight-indent-guides" "\
Automatically calculate indent guide faces.
If this feature is enabled, calculate reasonable values for the indent guide
colors based on the current theme's colorscheme, and set them appropriately.
This runs whenever a theme is loaded, but it can also be run interactively.

\(fn)" t nil)

(autoload 'highlight-indent-guides-mode "highlight-indent-guides" "\
Display indent guides in a buffer.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; highlight-indent-guides-autoloads.el ends here
