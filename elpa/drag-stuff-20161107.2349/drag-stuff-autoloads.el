;;; drag-stuff-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "drag-stuff" "drag-stuff.el" (23168 11300 963004
;;;;;;  980000))
;;; Generated autoloads from drag-stuff.el

(autoload 'drag-stuff-up "drag-stuff" "\
Drag stuff ARG lines up.

\(fn ARG)" t nil)

(autoload 'drag-stuff-down "drag-stuff" "\
Drag stuff ARG lines down.

\(fn ARG)" t nil)

(autoload 'drag-stuff-right "drag-stuff" "\
Drag stuff ARG lines to the right.

\(fn ARG)" t nil)

(autoload 'drag-stuff-left "drag-stuff" "\
Drag stuff ARG lines to the left.

\(fn ARG)" t nil)

(autoload 'drag-stuff-mode "drag-stuff" "\
Drag stuff around.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-drag-stuff-mode "drag-stuff" "\
Turn on `drag-stuff-mode'.

\(fn)" t nil)

(autoload 'turn-off-drag-stuff-mode "drag-stuff" "\
Turn off `drag-stuff-mode'.

\(fn)" t nil)

(defvar drag-stuff-global-mode nil "\
Non-nil if Drag-Stuff-Global mode is enabled.
See the `drag-stuff-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `drag-stuff-global-mode'.")

(custom-autoload 'drag-stuff-global-mode "drag-stuff" nil)

(autoload 'drag-stuff-global-mode "drag-stuff" "\
Toggle Drag-Stuff mode in all buffers.
With prefix ARG, enable Drag-Stuff-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Drag-Stuff mode is enabled in all buffers where
`turn-on-drag-stuff-mode' would do it.
See `drag-stuff-mode' for more information on Drag-Stuff mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("drag-stuff-pkg.el") (23168 11300 964406
;;;;;;  538000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; drag-stuff-autoloads.el ends here
