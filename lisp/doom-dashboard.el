;;; ui/dashboard/config.el -*- lexical-binding: t; -*-

(defvar dashboard-name "*dashboard*"
  "The name to use for the dashboard buffer.")

(defvar doom-fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")

(defvar dashboard-functions
  '(dashboard-widget-banner
    dashboard-widget-loaded
    dashboard-widget-shortmenu
    dashboard-widget-footer)
  "List of widget functions to run in the dashboard buffer to construct the
dashboard. These functions take no arguments and the dashboard buffer is current
while they run.")

(defvar dashboard-banner-file "spacemacs.png"
  "The path to the image file to be used in on the dashboard. The path is
relative to `dashboard-banner-dir'. If nil, always use the ASCII banner.")

(defvar dashboard-banner-dir (concat user-emacs-directory "/banner/")
  "Where to look for `dashboard-banner-file'.")

(defvar dashboard-banner-padding '(0 . 1)
  "Number of newlines to pad the banner with, above and below, respectively.")

(defvar dashboard-inhibit-refresh nil
  "If non-nil, the doom buffer won't be refreshed.")

(defvar dashboard-pwd-policy 'last-project
  "The policy to use when setting the `default-directory' in the dashboard.

Possible values:

  'last-project  the `doom-project-root' of the last open buffer
  'last          the `default-directory' of the last open buffer
  a FUNCTION     a function run with the `default-directory' of the last
                 open buffer, that returns a directory path
  a STRING       a fixed path
  nil            `default-directory' will never change")

(defvar dashboard-menu-sections
  '(("Open org-agenda"
     :icon (all-the-icons-octicon "calendar" :face 'dashboard-menu-title)
     :when (fboundp 'org-agenda)
     :action org-agenda)
    ("Open recent files"
     :icon (all-the-icons-octicon "file-text" :face 'dashboard-menu-title)
     :action recentf-open-files)
    ("Open project"
     :icon (all-the-icons-octicon "briefcase" :face 'dashboard-menu-title)
     :action projectile-switch-project)
    ("Jump to bookmark"
     :icon (all-the-icons-octicon "bookmark" :face 'dashboard-menu-title)
     :action bookmark-jump))
  "An alist of menu buttons used by `dashboard-widget-shortmenu'. Each
element is a cons cell (LABEL . PLIST). LABEL is a string to display after the
icon and before the key string.

PLIST can have the following properties:

  :icon FORM
    Uses the return value of FORM as an icon (can be literal string).
  :key STRING
    The keybind displayed next to the button.
  :when FORM
    If FORM returns nil, don't display this button.
  :face FACE
    Displays the icon and text with FACE (a face symbol).
  :action FORM
    Run FORM when the button is pushed.")

(defvar dashboard--last-cwd nil)
(defvar dashboard--width 80)
(defvar dashboard--old-fringe-indicator fringe-indicator-alist)
(defvar dashboard--pwd-alist ())
(defvar dashboard--reload-timer nil)
(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)

(defun doom-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `doom-fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create doom-fallback-buffer-name)))


(defun dashboard-init-h ()
  "Initializes dashboard."
  (unless noninteractive
    ;; Ensure the dashboard becomes Emacs' go-to buffer when there's nothing
    ;; else to show.
    (setq doom-fallback-buffer-name dashboard-name
          initial-buffer-choice #'doom-fallback-buffer)
    (unless fancy-splash-image
      (setq fancy-splash-image
            (expand-file-name dashboard-banner-file
                              dashboard-banner-dir)))
    (when (equal (buffer-name) "*scratch*")
      (set-window-buffer nil (doom-fallback-buffer))
      (if (daemonp)
          (add-hook 'after-make-frame-functions #'dashboard-reload-frame-h)
        (dashboard-reload)))
    ;; Ensure the dashboard is up-to-date whenever it is switched to or resized.
    (add-hook 'window-configuration-change-hook #'dashboard-resize-h)
    (add-hook 'window-size-change-functions #'dashboard-resize-h)
    (add-hook 'doom-switch-buffer-hook #'dashboard-reload-maybe-h)
    (add-hook 'delete-frame-functions #'dashboard-reload-frame-h)))

(add-hook 'emacs-startup-hook #'dashboard-init-h)

;;; Faces
(defgroup dashboard nil
  "Manage how dashboard is coloured and themed."
  :prefix "dashboard"
  :group 'doom-themes)

(defface dashboard-banner '((t (:inherit font-lock-comment-face)))
  "Face used for the DOOM banner on the dashboard"
  :group 'dashboard)

(defface dashboard-footer '((t (:inherit font-lock-keyword-face)))
  "Face used for the footer on the dashboard"
  :group 'dashboard)

(defface dashboard-footer-icon '((t (:inherit all-the-icons-green)))
  "Face used for the icon of the footer on the dashboard"
  :group 'dashboard)

(defface dashboard-loaded '((t (:inherit font-lock-comment-face)))
  "Face used for the loaded packages benchmark"
  :group 'dashboard)

(defface dashboard-menu-desc '((t (:inherit font-lock-constant-face)))
  "Face used for the key description of menu widgets on the dashboard"
  :group 'dashboard)

(defface dashboard-menu-title '((t (:inherit font-lock-keyword-face)))
  "Face used for the title of menu widgets on the dashboard"
  :group 'dashboard)

(define-derived-mode dashboard-mode special-mode
  (format "Dashboard v%s" "1.0")
  "Major mode for the DOOM dashboard buffer."
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq-local whitespace-style nil)
  (setq-local show-trailing-whitespace nil)
  (setq-local hscroll-margin 0)
  (setq-local tab-width 2)
  ;; Don't scroll to follow cursor
  (setq-local scroll-preserve-screen-position nil)
  (setq-local auto-hscroll-mode nil)
  (cl-loop for (car . _cdr) in fringe-indicator-alist
           collect (cons car nil) into alist
           finally do (setq fringe-indicator-alist alist))
  ;; Ensure point is always on a button
  (add-hook 'post-command-hook #'dashboard-reposition-point-h nil t))

(general-def dashboard-mode-map
  [left-margin mouse-1]   #'ignore
  "n"       #'forward-button
  "p"       #'backward-button
  "C-n"     #'forward-button
  "C-p"     #'backward-button
  [down]    #'forward-button
  [up]      #'backward-button
  [tab]     #'forward-button
  [backtab] #'backward-button
  ;; Evil remaps
  [remap evil-next-line]     #'forward-button
  [remap evil-previous-line] #'backward-button
  [remap evil-next-visual-line]     #'forward-button
  [remap evil-previous-visual-line] #'backward-button
  [remap evil-paste-pop-next] #'forward-button
  [remap evil-paste-pop]      #'backward-button
  [remap evil-delete]         #'ignore
  [remap evil-delete-line]    #'ignore
  [remap evil-insert]         #'ignore
  [remap evil-append]         #'ignore
  [remap evil-replace]        #'ignore
  [remap evil-replace-state]  #'ignore
  [remap evil-change]         #'ignore
  [remap evil-change-line]    #'ignore
  [remap evil-visual-char]    #'ignore
  [remap evil-visual-line]    #'ignore)

(defun dashboard-package-load-time ()
  ;; Check if package.el was loaded and if package loading was enabled
  (if (bound-and-true-p package-alist)
      (format "%d packages loaded in %s"
              (length package-activated-list) (emacs-init-time))
    (if (and (boundp 'straight--profile-cache) (hash-table-p straight--profile-cache))
        (format "%d packages loaded in %s"
                (hash-table-size straight--profile-cache) (emacs-init-time))
      (format "Emacs started in %s" (emacs-init-time)))))

(defun dashboard-reposition-point-h ()
  "Trap the point in the buttons."
  (when (region-active-p)
    (setq deactivate-mark t)
    (when (bound-and-true-p evil-local-mode)
      (evil-change-to-previous-state)))
  (or (ignore-errors
        (if (button-at (point))
            (forward-button 0)
          (backward-button 1)))
      (ignore-errors
        (goto-char (point-min))
        (forward-button 1))))

(defun dashboard-reload-maybe-h (&rest _)
  "Reload the dashboard or its state.

If this isn't a dashboard buffer, move along, but record its `default-directory'
if the buffer is real. See `doom-real-buffer-p' for an explanation for what
'real' means.

If this is the dashboard buffer, reload it completely."
  (cond ((dashboard-p (current-buffer))
         (let (dashboard-inhibit-refresh)
           (ignore-errors (dashboard-reload))))
        ((and (not (file-remote-p default-directory))
              (doom-real-buffer-p (current-buffer)))
         (setq dashboard--last-cwd default-directory)
         (dashboard-update-pwd))))

(defun dashboard-reload-frame-h (_frame)
  "Reload the dashboard after a brief pause. This is necessary for new frames,
whose dimensions may not be fully initialized by the time this is run."
  (when (timerp dashboard--reload-timer)
    (cancel-timer dashboard--reload-timer)) ; in case this function is run rapidly
  (setq dashboard--reload-timer (run-with-timer 0.1 nil #'dashboard-reload t)))

(defun dashboard-resize-h (&rest _)
  "Recenter the dashboard, and reset its margins and fringes."
  (let (buffer-list-update-hook
        window-configuration-change-hook
        window-size-change-functions)
    (let ((windows (get-buffer-window-list (doom-fallback-buffer) nil t)))
      (dolist (win windows)
        (set-window-start win 0)
        (set-window-fringes win 0 0)
        (set-window-margins
         win (max 0 (/ (- (window-total-width win) dashboard--width) 2))))
      (when windows
        (with-current-buffer (doom-fallback-buffer)
          (save-excursion
            (with-silent-modifications
              (goto-char (point-min))
              (delete-region (line-beginning-position)
                             (save-excursion (skip-chars-forward "\n")
                                             (point)))
              (insert (make-string
                       (+ (max 0 (- (/ (window-height (get-buffer-window)) 2)
                                    (round (/ (count-lines (point-min) (point-max))
                                              2))))
                          (car dashboard-banner-padding))
                       ?\n)))))))))

(defun dashboard--persp-detect-project-h (&rest _)
  "Check for a `last-project-root' parameter in the perspective, and set the
dashboard's `default-directory' to it if it exists.

This and `dashboard--persp-record-project-h' provides `persp-mode' integration with
the Doom dashboard. It ensures that the dashboard is always in the correct
project (which may be different across perspective)."
  (when (bound-and-true-p persp-mode)
    (when-let (pwd (persp-parameter 'last-project-root))
      (dashboard-update-pwd pwd))))

(defun dashboard--persp-record-project-h (&optional persp &rest _)
  "Record the last `doom-project-root' for the current perspective. See
`dashboard--persp-detect-project-h' for more information."
  (when (bound-and-true-p persp-mode)
    (set-persp-parameter
     'last-project-root (doom-project-root)
     (if (persp-p persp)
         persp
       (get-current-persp)))))


;;
;;; Library

(defun dashboard-p (buffer)
  "Returns t if BUFFER is the dashboard buffer."
  (eq buffer (get-buffer dashboard-name)))

(defun dashboard-update-pwd (&optional pwd)
  "Update `default-directory' in the Doom dashboard buffer. What it is set to is
controlled by `dashboard-pwd-policy'."
  (if pwd
      (with-current-buffer (doom-fallback-buffer)
        (setq-local default-directory pwd))
    (let ((new-pwd (dashboard--get-pwd)))
      (when (and new-pwd (file-accessible-directory-p new-pwd))
        (dashboard-update-pwd
         (concat (directory-file-name new-pwd)
                 "/"))))))

(defun dashboard-reload (&optional force)
  "Update the DOOM scratch buffer (or create it, if it doesn't exist)."
  (when (or (and (not dashboard-inhibit-refresh)
                 (get-buffer-window (doom-fallback-buffer))
                 (not (window-minibuffer-p (frame-selected-window))))
            force)
    (with-current-buffer (doom-fallback-buffer)
      ;; (doom-log "Reloading dashboard at %s" (format-time-string "%T"))
      (with-silent-modifications
        (let ((pt (point)))
          (unless (eq major-mode 'dashboard-mode)
            (dashboard-mode))
          (erase-buffer)
          (run-hooks 'dashboard-functions)
          (goto-char pt)
          (dashboard-reposition-point-h))
        (dashboard-resize-h)
        (dashboard--persp-detect-project-h)
        (dashboard-update-pwd)
        (current-buffer)))))

;; helpers
(defun dashboard--center (len s)
  (concat (make-string (ceiling (max 0 (- len (length s))) 2) ? )
          s))

(defun dashboard--get-pwd ()
  (let ((lastcwd dashboard--last-cwd)
        (policy dashboard-pwd-policy))
    (cond ((null policy)
           default-directory)
          ((stringp policy)
           (expand-file-name policy lastcwd))
          ((functionp policy)
           (funcall policy lastcwd))
          ((null lastcwd)
           default-directory)
          ((eq policy 'last-project)
           (let ((cwd default-directory))
             (or (doom-project-root lastcwd)
                 cwd)))
          ((eq policy 'last)
           lastcwd)
          ((warn "`dashboard-pwd-policy' has an invalid value of '%s'"
                 policy)))))

(defun dashboard-widget-banner ()
  (let ((point (point)))
    (mapc (lambda (line)
            (insert (propertize (dashboard--center dashboard--width line)
                                'face 'dashboard-banner) " ")
            (insert "\n"))
          '("=================     ===============     ===============   ========  ========"
            "\\\\ . . . . . . .\\\\   //. . . . . . .\\\\   //. . . . . . .\\\\  \\\\. . .\\\\// . . //"
            "||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\\/ . . .||"
            "|| . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||"
            "||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||"
            "|| . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\\ . . . . ||"
            "||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\\_ . .|. .||"
            "|| . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\\ `-_/| . ||"
            "||_-' ||  .|/    || ||    \\|.  || `-_|| ||_-' ||  .|/    || ||   | \\  / |-_.||"
            "||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \\  / |  `||"
            "||    `'         || ||         `'    || ||    `'         || ||   | \\  / |   ||"
            "||            .===' `===.         .==='.`===.         .===' /==. |  \\/  |   ||"
            "||         .=='   \\_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \\/  |   ||"
            "||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \\/  |   ||"
            "||   .=='    _-'          '-__\\._-'         '-_./__-'         `' |. /|  |   ||"
            "||.=='    _-'                                                     `' |  /==.||"
            "=='    _-'                         E M A C S                          \\/   `=="
            "\\   _-'                                                                `-_   /"
            " `''                                                                      ``'"))
    (when (and (display-graphic-p)
               (stringp fancy-splash-image)
               (file-readable-p fancy-splash-image))
      (let ((image (create-image (fancy-splash-image-file))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- dashboard--width
                                      (car (image-size image nil)))
                                   2))))
                   ? ))))
      (insert (make-string (or (cdr dashboard-banner-padding) 0)
                           ?\n)))))

(defun dashboard-widget-loaded ()
  (insert
   "\n"
   (propertize
    (dashboard--center
     dashboard--width
     (dashboard-package-load-time))
    'face 'dashboard-loaded)
   "\n\n"))

(defun dashboard-widget-shortmenu ()
  (let ((all-the-icons-scale-factor 1.45)
        (all-the-icons-default-adjust -0.02))
    (insert "\n")
    (dolist (section dashboard-menu-sections)
      (cl-destructuring-bind (label &key icon action when face) section
        (when (and (fboundp action)
                   (or (null when)
                       (eval when t)))
          (insert
           (dashboard--center
            (- dashboard--width 1)
            (let ((icon (if (stringp icon) icon (eval icon t))))
              (format (format "%s%%s%%-10s" (if icon "%3s\t" "%3s"))
                      (or icon "")
                      (with-temp-buffer
                        (insert-text-button
                         label
                         'action
                         `(lambda (_)
                            (call-interactively (or (command-remapping #',action)
                                                    #',action)))
                         'face (or face 'dashboard-menu-title)
                         'follow-link t
                         'help-echo
                         (format "%s (%s)" label
                                 (propertize (symbol-name action) 'face 'dashboard-menu-desc)))
                        (format "%-37s" (buffer-string)))
                      ;; Lookup command keys dynamically
                      (or (when-let (key (where-is-internal action nil t))
                            (with-temp-buffer
                              (save-excursion (insert (key-description key)))
                              (while (re-search-forward "<\\([^>]+\\)>" nil t)
                                (let ((str (match-string 1)))
                                  (replace-match
                                   (upcase (if (< (length str) 3)
                                               str
                                             (substring str 0 3))))))
                              (propertize (buffer-string) 'face 'dashboard-menu-desc)))
                          ""))))
           (if (display-graphic-p)
               "\n\n"
             "\n")))))))

(defun dashboard-footer-message ()
  (propertize (format "Powered by %s, %s  " user-full-name (format-time-string "%Y"))
              'face 'dashboard-loaded))

(defun dashboard-widget-footer ()
  (insert
   "\n"
   (dashboard--center
    (- dashboard--width 2)
    (with-temp-buffer
      (insert (dashboard-footer-message))
      (insert-text-button (or (all-the-icons-octicon "octoface" :face 'dashboard-footer-icon :height 1.3 :v-adjust -0.15)
                              (propertize "github" 'face 'dashboard-footer))
                          'action (lambda (_) (browse-url "https://github.com/lkzz/emacs.d"))
                          'follow-link t
                          'help-echo "Open Emacs github page")
      (buffer-string)))
   "\n"))

(provide 'doom-dashboard)
