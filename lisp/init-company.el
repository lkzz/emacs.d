;;; init-company.el --- auto complate use company. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2017-2021  Kevin Leung
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(use-package company
  :diminish company-mode "ⓒ"
  :general
  (company-active-map "C-s" 'company-filter-candidates
                      "C-p" 'company-select-previous
                      "C-n" 'company-select-next
                      "C-u" 'company-previous-page
                      "C-d" 'company-next-page
                      "<tab>" 'company-complete-common-or-cycle)
  (company-search-map "C-p" 'company-select-previous
                      "C-n" 'company-select-next)
  :hook (after-init . global-company-mode)
  :init
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-echo-delay 0            ; remove annoying blinking
        company-idle-delay 0.0          ; set the completion menu pop-up delay
        company-minimum-prefix-length 1 ; pop up a completion menu by tapping a character
        company-show-quick-access t     ; display numbers on the left
        company-tooltip-limit 10
        company-require-match nil
        company-selection-wrap-around t     ; make previous/next selection in the popup cycle
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
        company-backends '((company-capf :with company-tabnine :separate)
                           (company-dabbrev company-keywords company-files)))
  :config
  (add-hook 'evil-normal-state-entry-hook (lambda ()
                                            (when company-candidates (company-abort))))
  ;; Remove duplicate candidate.
  (add-to-list 'company-transformers #'delete-dups)

  (defun my-lsp-fix-company-capf ()
    "Remove redundant `company-capf'."
    (setq company-backends
          (remove 'company-backends (remq 'company-capf company-backends))))
  (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

  ;; Uses machine learning to provide suggestions.
  (use-package company-tabnine
    :config
    (setq company-tabnine-max-num-results 3)
    ;; The free version of TabNine is good enough,
    ;; and below code is recommended that TabNine not always
    ;; prompt me to purchase a paid version in a large project.
    (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
      (let ((company-message-func (ad-get-arg 0)))
        (when (and company-message-func
                   (stringp (funcall company-message-func)))
          (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
            ad-do-it))))

    (defun company//sort-by-tabnine (candidates)
      "实现前2个候选项是company-capf的,接着的2个是TabNine的."
      (if (or (functionp company-backend)
              (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
          candidates
        (let ((candidates-table (make-hash-table :test #'equal))
              candidates-1
              candidates-2)
          (dolist (candidate candidates)
            (if (eq (get-text-property 0 'company-backend candidate)
                    'company-tabnine)
                (unless (gethash candidate candidates-table)
                  (push candidate candidates-2))
              (push candidate candidates-1)
              (puthash candidate t candidates-table)))
          (setq candidates-1 (nreverse candidates-1))
          (setq candidates-2 (nreverse candidates-2))
          (nconc (seq-take candidates-1 2)
                 (seq-take candidates-2 2)
                 (seq-drop candidates-1 2)
                 (seq-drop candidates-2 2)))))
    (add-to-list 'company-transformers 'company//sort-by-tabnine t))

  ;; Simple but effective sorting and filtering for Emacs.
  (use-package company-prescient
    :hook (company-mode . company-prescient-mode)
    :config (prescient-persist-mode +1))

  (use-package company-box
    :if (display-graphic-p)
    :diminish
    :defines company-box-icons-all-the-icons
    :hook (company-mode . company-box-mode)
    :init (setq company-box-backends-colors nil
                company-box-doc-delay 0.1)
    :config
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'lisp-mode))
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)
    ;; Display borders
    (defun my-company-box--display (string on-update)
      "Display the completions."
      (company-box--render-buffer string on-update)
      (let ((frame (company-box--get-frame))
            (border-color (face-foreground 'font-lock-comment-face nil t)))
        (unless frame
          (setq frame (company-box--make-frame))
          (company-box--set-frame frame))
        (company-box--compute-frame-position frame)
        (company-box--move-selection t)
        (company-box--update-frame-position frame)
        (unless (frame-visible-p frame)
          (make-frame-visible frame))
        (company-box--update-scrollbar frame t)
        (set-face-background 'internal-border border-color frame)
        (when (facep 'child-frame-border)
          (set-face-background 'child-frame-border border-color frame)))
      (with-current-buffer (company-box--get-buffer)
        (company-box--maybe-move-number (or company-box--last-start 1))))
    (advice-add #'company-box--display :override #'my-company-box--display)

    (setq company-box-doc-frame-parameters '((internal-border-width . 1)
                                             (left-fringe . 10)
                                             (right-fringe . 10)))
    (defun my-company-box-doc--make-buffer (object)
      (let* ((buffer-list-update-hook nil)
             (inhibit-modification-hooks t)
             (string (cond ((stringp object) object)
                           ((bufferp object) (with-current-buffer object (buffer-string))))))
        (when (and string (> (length (string-trim string)) 0))
          (with-current-buffer (company-box--get-buffer "doc")
            (erase-buffer)
            (insert (propertize "\n" 'face '(:height 0.5)))
            (insert string)
            (insert (propertize "\n\n" 'face '(:height 0.5)))
            (setq mode-line-format nil
                  display-line-numbers nil
                  header-line-format nil
                  show-trailing-whitespace nil
                  cursor-in-non-selected-windows nil)
            (current-buffer)))))
    (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

    (defun my-company-box-doc--show (selection frame)
      (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
                (window-configuration-change-hook nil)
                (inhibit-redisplay t)
                (display-buffer-alist nil)
                (buffer-list-update-hook nil))
        (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                       company-box--bottom
                                       company-selection
                                       (company-box--get-frame)
                                       (frame-visible-p (company-box--get-frame))))
                     (candidate (nth selection company-candidates))
                     (doc (or (company-call-backend 'quickhelp-string candidate)
                              (company-box-doc--fetch-doc-buffer candidate)))
                     (doc (company-box-doc--make-buffer doc)))
          (let ((frame (frame-local-getq company-box-doc-frame))
                (border-color (face-foreground 'font-lock-comment-face nil t)))
            (unless (frame-live-p frame)
              (setq frame (company-box-doc--make-frame doc))
              (frame-local-setq company-box-doc-frame frame))
            (set-face-background 'internal-border border-color frame)
            (when (facep 'child-frame-border)
              (set-face-background 'child-frame-border border-color frame))
            (company-box-doc--set-frame-position frame)
            (unless (frame-visible-p frame)
              (make-frame-visible frame))))))
    (advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

    (defun my-company-box-doc--set-frame-position (frame)
      (-let* ((frame-resize-pixelwise t)
              (box-frame (company-box--get-frame))
              (box-position (frame-position box-frame))
              (box-width (frame-pixel-width box-frame))
              (box-height (frame-pixel-height box-frame))
              (box-border-width (frame-border-width box-frame))
              (window (frame-root-window frame))
              ((text-width . text-height) (window-text-pixel-size window nil nil
                                                                  (/ (frame-pixel-width) 2)
                                                                  (- (frame-pixel-height) 50)))
              (border-width (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0))
              (x (- (+ (car box-position) box-width) border-width))
              (space-right (- (frame-pixel-width) x))
              (space-left (car box-position))
              (fringe-left (or (alist-get 'left-fringe company-box-doc-frame-parameters) 0))
              (fringe-right (or (alist-get 'right-fringe company-box-doc-frame-parameters) 0))
              (width (+ text-width border-width fringe-left fringe-right))
              (x (or (and (> width space-right)
                          (> space-left width)
                          (- space-left width))
                     x))
              (y (cdr box-position))
              (bottom (+ company-box--bottom (window-pixel-top) (frame-border-width)))
              (height (+ text-height (* 2 border-width)))
              (y (if (> (+ y height) bottom)
                     (- (+ y box-height) height)
                   y)))
        (set-frame-position frame (max x 0) (max y 0))
        (set-frame-size frame text-width text-height t)))
    (advice-add #'company-box-doc--set-frame-position :override #'my-company-box-doc--set-frame-position)

    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 1.0 :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width" :height 1.0 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 1.0 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 1.0 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 1.0 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 1.0 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 1.0 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 1.0 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 1.0 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 1.0 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2))
            (Constant . ,(all-the-icons-faicon "square-o" :height 1.0 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 1.0 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 1.0 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 1.0 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 1.0 :v-adjust -0.2)))
          company-box-icons-alist 'company-box-icons-all-the-icons))

  ;; Display documentation for completion candidates in terminal
  (use-package company-quickhelp-terminal
    :unless (display-graphic-p)
    :defines company-quickhelp-delay
    :bind (:map company-active-map
           ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
    :hook ((global-company-mode . company-quickhelp-mode)
           (company-quickhelp-mode  . company-quickhelp-terminal-mode))
    :init (setq company-quickhelp-delay 0.3)))

(provide 'init-company)
;;; init-company.el ends here
