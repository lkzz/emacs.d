;;; counsel-gtags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "counsel-gtags" "counsel-gtags.el" (23157 33538
;;;;;;  323782 836000))
;;; Generated autoloads from counsel-gtags.el

(autoload 'counsel-gtags-find-definition "counsel-gtags" "\
Search for TAGNAME definition in tag database.
Prompt for TAGNAME if not given.

\(fn TAGNAME)" t nil)

(autoload 'counsel-gtags-find-reference "counsel-gtags" "\
Search for TAGNAME reference in tag database.
Prompt for TAGNAME if not given.

\(fn TAGNAME)" t nil)

(autoload 'counsel-gtags-find-symbol "counsel-gtags" "\
Search for TAGNAME symbol in tag database.
Prompt for TAGNAME if not given.

\(fn TAGNAME)" t nil)

(autoload 'counsel-gtags-find-file "counsel-gtags" "\
Search for FILENAME among tagged files.
Prompt for FILENAME if not given.

\(fn FILENAME)" t nil)

(autoload 'counsel-gtags-go-backward "counsel-gtags" "\
Go to previous position in context stack.

\(fn)" t nil)

(autoload 'counsel-gtags-go-forward "counsel-gtags" "\
Go to next position in context stack.

\(fn)" t nil)

(autoload 'counsel-gtags-create-tags "counsel-gtags" "\
Create tag database in ROOTDIR.
LABEL is passed as the value for the environment variable GTAGSLABEL.
Prompt for ROOTDIR and LABEL if not given.  This command is asynchronous.

\(fn ROOTDIR LABEL)" t nil)

(autoload 'counsel-gtags-update-tags "counsel-gtags" "\
Update tag database for current file.
Changes in other files are ignored.  With a prefix argument, update
tags for all files.  With two prefix arguments, generate new tag
database in prompted directory.

\(fn)" t nil)

(autoload 'counsel-gtags-dwim "counsel-gtags" "\
Find definition or reference of thing at point (Do What I Mean).
If point is at a definition, find its references, otherwise, find
its definition.

\(fn)" t nil)

(autoload 'counsel-gtags-mode "counsel-gtags" "\
Toggle Counsel-Gtags mode on or off.
With a prefix argument ARG, enable Counsel-Gtags mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{counsel-gtags-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; counsel-gtags-autoloads.el ends here
