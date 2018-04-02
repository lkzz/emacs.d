;;; init-persp-mode.el --- Initialize persp mode
;;; Commentary:
;;; Code:

(defvar kevin/default-layout-name "Default"
  "Name of the default layout.")

(use-package persp-mode
  :ensure t
  :defer t
  :diminish persp-mode
  :init
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1)))
  (defhydra hydra-persp-mode (:body-pre (unless (bound-and-true-p persp-mode)
                                          (call-interactively 'persp-mode))
                                        :color blue
                                        :hint nil)
    "
\n
 Go to^^^^^^                                  Actions^^
 ─────^^^^^^────────────────────────────────  ───────^^──────────────────────────────────────────────────
 [_0_.._9_]^^     nth/new layout              [_a_]^^   add buffer
 [_C-0_.._C-9_]^^ nth/new layout              [_A_]^^   add all from layout
 [_<tab>_]^^^^    last layout                 [_d_]^^   close current layout
 [_b_]^^^^        buffer in layout            [_D_]^^   close other layout
 [_h_]^^^^        default layout              [_L_]^^   load layouts from file
 [_l_]^^^^        layout w/helm/ivy           [_r_]^^   remove current buffer
 [_n_/_C-l_]^^    next layout                 [_R_]^^   rename current layout
 [_N_/_p_/_C-h_]  prev layout                 [_s_/_S_] save all layouts/save by names
 [_o_]^^^^        custom layout               [_t_]^^   show a buffer without adding it to current layout
 [_w_]^^^^        workspaces transient state  [_x_]^^   kill current w/buffers
 ^^^^^^                                       [_X_]^^   kill other w/buffers
 ^^^^^^                                       [_<_/_>_] move layout left/right
 ^^^^^^                                       [_?_]^^   toggle help\n
"
    ("?" spacemacs//layouts-ts-toggle-hint)
    ("1" spacemacs/persp-switch-to-1 :exit t)
    ("2" spacemacs/persp-switch-to-2 :exit t)
    ("3" spacemacs/persp-switch-to-3 :exit t)
    ("4" spacemacs/persp-switch-to-4 :exit t)
    ("5" spacemacs/persp-switch-to-5 :exit t)
    ("6" spacemacs/persp-switch-to-6 :exit t)
    ("7" spacemacs/persp-switch-to-7 :exit t)
    ("8" spacemacs/persp-switch-to-8 :exit t)
    ("9" spacemacs/persp-switch-to-9 :exit t)
    ("0" spacemacs/persp-switch-to-0 :exit t)
    ("C-1" spacemacs/persp-switch-to-1)
    ("C-2" spacemacs/persp-switch-to-2)
    ("C-3" spacemacs/persp-switch-to-3)
    ("C-4" spacemacs/persp-switch-to-4)
    ("C-5" spacemacs/persp-switch-to-5)
    ("C-6" spacemacs/persp-switch-to-6)
    ("C-7" spacemacs/persp-switch-to-7)
    ("C-8" spacemacs/persp-switch-to-8)
    ("C-9" spacemacs/persp-switch-to-9)
    ("C-0" spacemacs/persp-switch-to-0)
    ("<tab>" spacemacs/jump-to-last-layout)
    ("<return>" nil :exit t)
    ("TAB" spacemacs/jump-to-last-layout)
    ("RET" nil :exit t)
    ("C-h" persp-prev)
    ("C-l" persp-next)
    ("<" spacemacs/move-current-persp-left)
    (">" spacemacs/move-current-persp-right)
    ("a" persp-add-buffer :exit t)
    ("A" persp-import-buffers :exit t)
    ("b" spacemacs/persp-buffers :exit t)
    ("d" spacemacs/layouts-ts-close)
    ("D" spacemacs/layouts-ts-close-other :exit t)
    ("h" spacemacs/layout-goto-default :exit t)
    ("L" persp-load-state-from-file :exit t)
    ("l" spacemacs/persp-perspectives :exit t)
    ("n" persp-next)
    ("N" persp-prev)
    ("o" spacemacs/select-custom-layout :exit t)
    ("p" persp-prev)
    ("r" persp-remove-buffer :exit t)
    ("R" spacemacs/layouts-ts-rename :exit t)
    ("s" persp-save-state-to-file :exit t)
    ("S" persp-save-to-file-by-names :exit t)
    ("t" persp-temporarily-display-buffer :exit t)
    ("w" spacemacs/workspaces-transient-state/body :exit t)
    ("x" spacemacs/layouts-ts-kill)
    ("X" spacemacs/layouts-ts-kill-other :exit t))

  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-name kevin/default-layout-name
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (concat kevin/cache-directory "workspaces/")
        persp-set-last-persp-for-new-frames nil
        persp-switch-to-added-buffer nil
        persp-remove-buffers-from-nil-persp-behaviour nil
        ;; Don't restore winconf on new frames
        persp-init-frame-behaviour t
        persp-init-new-frame-behaviour-override 'auto-temp
        ;; Auto-load on startup
        persp-auto-resume-time 3
        ;; auto-save on kill
        persp-auto-save-opt (if noninteractive 0 1))
  )

(defvar spacemacs--last-selected-layout kevin/default-layout-name
  "Previously selected layout.")

(defvar spacemacs--persp-display-buffers-func 'ignore
  "Function to display buffers in the perspective.")

(defvar spacemacs--custom-layout-alist nil
  "List of custom layouts with their bound keys.
Do not modify directly, use provided `spacemacs|define-custom-layout'")

(defvar spacemacs--persp-display-perspectives-func 'ignore
  "Function to display perspectives.")

(defvar dotspacemacs-auto-generate-layout-names nil
  "Layout name.")

(defvar spacemacs-generic-layout-names
  '(("zebra" "zucchini" "zen" "yellow" "yeti" "yard") ; grab-bag
    ("baboon" "banana" "blue")                        ; 2nd layout
    ("crab" "cabbage" "crayon")                       ; 3rd
    ("deer" "doughnut" "door")                        ; 4th
    ("elephant" "eggplant" "extreme")                 ; 5th
    ("falcon" "fig" "fjord")                          ; 6th
    ("gnu" "garlic" "guardian")                       ; 7th
    ("horse" "honey" "hallelujah")                    ; 8th
    ("iguana" "ice-cream" "internet")                 ; 9th
    ("jellyfish" "jalapeno" "jolt"))                  ; 10th (aka 0th)
  "Names for auto-generated layout names.
Used by `spacemacs//generate-layout-name'.

Must be a list with 10 entries, where each entry is a list of
names.  The 2nd list contains possible names for the 2nd
layout (or 10th) layout, the 3rd list contains names for the 3rd
layout, the 4th for the 4th, and so on until the 10th (aka layout
number 0).  The first list is sepcial - it is a grab-bag for names
in case none of the regular names can be used for a new layout.")

(defvar spacemacs-buffer--last-width nil
  "Previous width of spacemacs-buffer.")

(defun spacemacs//current-layout-name ()
  "Get name of the current perspective."
  (safe-persp-name (get-frame-persp)))

(defun spacemacs//layout-not-contains-buffer-p (buffer)
  "Return non-nil if current layout doesn't contain BUFFER."
  (not (persp-contain-buffer-p buffer)))

(defun spacemacs/jump-to-last-layout ()
  "Open the previously selected layout, if it exists."
  (interactive)
  (unless (eq 'non-existent
              (gethash spacemacs--last-selected-layout
                       *persp-hash* 'non-existent))
    (persp-switch spacemacs--last-selected-layout)))

(defun spacemacs-buffer/refresh ()
  "Force recreation of the spacemacs buffer."
  (interactive)
  (setq spacemacs-buffer--last-width nil)
  (spacemacs-buffer/goto-buffer t))

(defalias 'spacemacs/home 'kevin/revert-buffer-no-confirm
  "Go to home Spacemacs buffer")

;; (defalias 'spacemacs/home 'spacemacs-buffer/refresh
;;   "Go to home Spacemacs buffer")

(defun spacemacs/persp-buffers ()
  "Call the function defined in `spacemacs--persp-display-buffers-func'."
  (interactive)
  (call-interactively spacemacs--persp-display-buffers-func))

(defun spacemacs/persp-perspectives ()
  "Call the function defined in `spacemacs--persp-display-perspectives-func'."
  (interactive)
  (call-interactively spacemacs--persp-display-perspectives-func))

(defun spacemacs//layout-format-name (name pos)
  "Format the layout name given by NAME and POS for display in mode-line."
  (let* ((layout-name (if (file-directory-p name)
                          (file-name-nondirectory (directory-file-name name))
                        name))
         (string-name (format "%s" layout-name))
         (current (equal name (spacemacs//current-layout-name)))
         (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                          ":" string-name)))
    (if current
        (propertize (concat "[" caption "]") 'face 'warning)
      caption)))

(defun spacemacs//generate-layout-name (pos)
  "Generate name for layout of position POS.
POS should be a number between 1 and 9, where 1 represents the
2nd layout, 2 represents the 3rd and so on, 9 represents the 10th
layout, which is also knows as the 0th layout.

 If no name can be generated, return nil."
  (catch 'found
    ;; return 1st available name
    (dolist (name (nth pos spacemacs-generic-layout-names))
      (unless (persp-p (persp-get-by-name name))
        (throw 'found name)))

    ;; return 1st available name from grab-bag
    (dolist (name (car spacemacs-generic-layout-names))
      (unless (persp-p (persp-get-by-name name))
        (throw 'found name)))))

(defun spacemacs/layout-switch-by-pos (pos)
  "Switch to perspective of position POS.
If POS has no layout, and `dotspacemacs-auto-generate-layout-names'
is non-nil, create layout with auto-generated name.  Otherwise,
ask the user if a new layout should be created."
  (let ((persp-to-switch
         (nth pos (persp-names-current-frame-fast-ordered))))
    (if persp-to-switch
        (persp-switch persp-to-switch)
      (let ((persp-reset-windows-on-nil-window-conf t)
            (generated-name (and dotspacemacs-auto-generate-layout-names
                                 (spacemacs//generate-layout-name pos))))
        (cond
         (generated-name
          (persp-switch generated-name))
         ((y-or-n-p (concat "Layout in this position doesn't exist. "
                            "Do you want to create one? "))
          (persp-switch nil)
          (spacemacs-buffer/refresh-delete-other-windows)))))))

;; Define all `spacemacs/persp-switch-to-X' functions
(dolist (i (number-sequence 9 0 -1))
  (eval `(defun ,(intern (format "spacemacs/persp-switch-to-%s" i)) nil
           ,(format "Switch to layout %s.\n%s"
                    i "See `spacemacs/layout-switch-by-pos' for details.")
           (interactive)
           (spacemacs/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

(defun spacemacs/layout-goto-default ()
  "Go to `kevin/default-layout-name` layout."
  (interactive)
  (when kevin/default-layout-name
    (persp-switch kevin/default-layout-name)))

(defun spacemacs/layouts-ts-rename ()
  "Rename a layout and get back to the perspectives transient-state."
  (interactive)
  (call-interactively 'persp-rename)
  (hydra-persp-mode/body))

(defun spacemacs/layouts-ts-close ()
  "Kill current perspective."
  (interactive)
  (persp-kill-without-buffers (spacemacs//current-layout-name)))

(defun spacemacs/layouts-ts-close-other ()
  "Close other perspective."
  (interactive)
  (call-interactively 'spacemacs/helm-persp-close)
  (hydra-persp-mode/body))

(defun spacemacs/layouts-ts-kill ()
  "Kill current perspective."
  (interactive)
  (persp-kill (spacemacs//current-layout-name)))

(defun spacemacs/layouts-ts-kill-other ()
  "Kill other perspective."
  (interactive)
  (call-interactively 'spacemacs/helm-persp-kill)
  (hydra-persp-mode/body))

;; ability to use helm find files but also adds to current perspective
(defun spacemacs/helm-persp-close ()
  "Kill perspectives without killing the buffers."
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives (without killing buffers)*"
   :sources
   (helm-build-in-buffer-source
    (concat "Current Perspective: " (spacemacs//current-layout-name))
    :data (persp-names)
    :fuzzy-match t
    :action
    '(("Close perspective(s)" . (lambda (candidate)
                                  (mapcar
                                   'persp-kill-without-buffers
                                   (helm-marked-candidates))))))))

(defun spacemacs/helm-persp-kill ()
  "Kill perspectives with all their buffers."
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives with all their buffers*"
   :sources (helm-build-in-buffer-source
             (s-concat "Current Perspective: "
                       (spacemacs//current-layout-name))
             :data (persp-names)
             :fuzzy-match t
             :action
             '(("Kill perspective(s)" .
                (lambda (candidate)
                  (mapcar 'persp-kill
                          (helm-marked-candidates))))))))

(defun spacemacs/move-element-left (element list)
  "Move ELEMENT one step to the left in LIST."
  (let (value)
    (dolist (name list value)
      (if (and (equal name element) (car value))
          (setq value (cons (car value) (cons name (cdr value))))
        (setq value (cons name value))))
    (nreverse value)))

(defun spacemacs/move-element-right (element list)
  "Move ELEMENT one step to the right in LIST."
  (nreverse (spacemacs/move-element-left element (reverse list))))

(defun spacemacs/move-current-persp-right ()
  "Move the current perspective one step to the right."
  (interactive)
  (setq persp-names-cache (spacemacs/move-element-right
                           (spacemacs//current-layout-name)
                           persp-names-cache)))

(defun spacemacs/move-current-persp-left ()
  "Move the current perspective one step to the left."
  (interactive)
  (setq persp-names-cache (spacemacs/move-element-left
                           (spacemacs//current-layout-name)
                           persp-names-cache)))

;; Custom Persp transient-state

(defun spacemacs//custom-layout-func-name (name)
  "Return the name of the custom-perspective function for NAME."
  (intern (concat "spacemacs/custom-perspective-" name)))

(defmacro spacemacs|define-custom-layout (name &rest props)
  "Define a custom-perspective called NAME.

FUNC is a FUNCTION defined using NAME and the result of
`spacemacs//custom-layout-func-name', it takes care of
creating the perspective NAME and executing the expressions given
in the :body property to this macro.

NAME is a STRING.

Available PROPS:

`:binding STRING'
   Key to be bound to the function FUNC

`:body EXPRESSIONS'
  One or several EXPRESSIONS that are going to be evaluated after
  we change into the perspective NAME."
  (declare (indent 1))
  (let* ((name (if (symbolp name)
                   (symbol-value name)
                 name))
         (func (spacemacs//custom-layout-func-name name))
         (binding-prop (car (spacemacs/mplist-get props :binding)))
         (binding (if (symbolp binding-prop)
                      (symbol-value binding-prop)
                    binding-prop))
         (body (spacemacs/mplist-get props :body))
         (already-defined? (cdr (assoc binding
                                       spacemacs--custom-layout-alist))))
    `(progn
       (defun ,func ()
         ,(format "Open custom perspective %s" name)
         (interactive)
         (let ((initialize (not (gethash ,name *persp-hash*))))
           (persp-switch ,name)
           (when initialize
             (delete-other-windows)
             ,@body)))
       ;; Check for Clashes
       (if ,already-defined?
           (unless (equal ,already-defined? ,name)
             (spacemacs-buffer/message "Replacing existing binding \"%s\" for %s with %s"
                                       ,binding ,already-defined? ,name)
             (setq spacemacs--custom-layout-alist
                   (delete (assoc ,binding spacemacs--custom-layout-alist)
                           spacemacs--custom-layout-alist))
             (push '(,binding . ,name) spacemacs--custom-layout-alist))
         (push '(,binding . ,name) spacemacs--custom-layout-alist)))))

(defun spacemacs/select-custom-layout ()
  "Update the custom-perspectives transient-state and then activate it."
  (interactive)
  (spacemacs//update-custom-layouts)
  (spacemacs/custom-layouts-transient-state/body))

(defun spacemacs//custom-layouts-ms-documentation ()
  "Return the docstring for the custom perspectives transient-state."
  (if spacemacs--custom-layout-alist
      (mapconcat (lambda (custom-persp)
                   (format "[%s] %s"
                           (car custom-persp) (cdr custom-persp)))
                 spacemacs--custom-layout-alist " ")
    (spacemacs-buffer/warning (format "`spacemacs--custom-layout-alist' variable is empty" ))))

(defun spacemacs//update-custom-layouts ()
  "Ensure the custom-perspectives transient-state is updated.
Takes each element in the list `spacemacs--custom-layout-alist'
format so they are supported by the
`spacemacs/custom-layouts-transient-state' macro."
  (let (bindings)
    (dolist (custom-persp spacemacs--custom-layout-alist bindings)
      (let* ((binding (car custom-persp))
             (name (cdr custom-persp))
             (func-name (spacemacs//custom-layout-func-name name)))
        (push (list binding func-name :exit t) bindings)))
    (eval `(spacemacs|define-transient-state custom-layouts
                                             :doc (concat (spacemacs//custom-layouts-ms-documentation))
                                             :bindings
                                             ,@bindings))))

;; Ivy integration
(defun spacemacs/ivy-persp-switch-project-advice (project)
  (let ((persp-reset-windows-on-nil-window-conf t))
    (persp-switch project)))

(defun spacemacs/ivy-persp-switch-project (arg)
  (interactive "P")
  (require 'counsel-projectile)
  (advice-add 'counsel-projectile-switch-project-action
              :before #'spacemacs/ivy-persp-switch-project-advice)
  (ivy-read "Switch to Project Perspective: "
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects)
            :action #'counsel-projectile-switch-project-action
            :caller 'spacemacs/ivy-persp-switch-project)
  (advice-remove 'counsel-projectile-switch-project-action
                 'spacemacs/ivy-persp-switch-project-advice))

(defun spacemacs-buffer/refresh-delete-other-windows ()
  "Open home Spacemacs buffer and delete other windows.
Useful for making the home buffer the only visible buffer in the frame."
  (interactive)
  (spacemacs/home)
  (delete-other-windows))

(provide 'init-persp-mode)
;;; init-persp-mode.el ends here
