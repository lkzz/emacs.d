;;; swift-mode.el --- Major-mode for Apple's Swift programming language. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017 taku0, Chris Barrett, Bozhidar Batsov, Arthur Evstifeev

;; Authors: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;;
;; Version: 4.1.0
;; Package-Requires: ((emacs "24.4") (seq "2.3"))
;; Keywords: languages swift
;; URL: https://github.com/swift-emacs/swift-mode

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major-mode for Apple's Swift programming language.

;;; Code:

(require 'swift-mode-lexer)
(require 'swift-mode-indent)
(require 'swift-mode-font-lock)
(require 'swift-mode-beginning-of-defun)
(require 'swift-mode-repl)

;;;###autoload
(defgroup swift nil
  "Major-mode for Apple's Swift programming language."
  :group 'languages
  :prefix "swift-mode:")

;;; Keymap

(defvar swift-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map (kbd "M-j") #'swift-mode:indent-new-comment-line)
    (define-key map (kbd "C-M-j") #'swift-mode:indent-new-comment-line)
    (define-key map (kbd "C-c C-z") #'swift-mode:run-repl)
    (define-key map (kbd "C-c C-f") #'swift-mode:send-buffer)
    (define-key map (kbd "C-c C-r") #'swift-mode:send-region)
    (define-key map (kbd "C-M-a") #'swift-mode:beginning-of-defun)
    (define-key map (kbd "C-M-e") #'swift-mode:end-of-defun)
    (define-key map (kbd "<C-M-home>") #'swift-mode:beginning-of-defun)
    (define-key map (kbd "<C-M-end>") #'swift-mode:end-of-defun)
    (define-key map (kbd "ESC <C-home>") #'swift-mode:beginning-of-defun)
    (define-key map (kbd "ESC <C-end>") #'swift-mode:end-of-defun)
    (define-key map (kbd "C-M-h") #'swift-mode:mark-defun)
    (define-key map (kbd "C-x n d") #'swift-mode:narrow-to-defun)
    (define-key map (kbd "M-a") #'swift-mode:backward-sentence)
    (define-key map (kbd "M-e") #'swift-mode:forward-sentence)
    (define-key map (kbd "M-k") #'swift-mode:kill-sentence)
    (define-key map (kbd "C-x DEL") #'swift-mode:backward-kill-sentence)
    ;; (define-key map (kbd "???") #'swift-mode:mark-sentence)
    (define-key map (kbd "C-x n s") #'swift-mode:narrow-to-sentence)

    (easy-menu-define swift-menu map "Swift Mode menu"
      `("Swift"
        :help "Swift-specific Features"
        ["Run REPL" swift-mode-run-repl
         :help "Run Swift REPL"]
        ["Send buffer to REPL" swift-mode:send-buffer
         :help "Send the current buffer's contents to the REPL"]
        ["Send region to REPL" swift-mode:send-region
         :help "Send currently selected region to the REPL"]
        ["Build Swift module" swift-mode:build-swift-module
         :help "Build current Swift module"]
        ["Build iOS app" swift-mode:build-ios-app
         :help "Build current iOS app"]
        ["Debug Swift module" swift-mode:debug-swift-module
         :help "Debug current Swift module"]
        ["Debug iOS app" swift-mode:debug-ios-app
         :help "Debug current iOS app with simulator"]))
    map)
  "Swift mode key map.")

;;; `forward-sexp-function'

(defun swift-mode:forward-sexp (&optional arg)
  "Move forward/backward a token or list.

See `forward-sexp for ARG."
  (setq arg (or arg 1))
  (when (swift-mode:chunk-after)
    (goto-char (swift-mode:chunk:start (swift-mode:chunk-after))))
  (if (< 0 arg)
      (while (< 0 arg)
        (while (eq (swift-mode:token:type (swift-mode:forward-sexp-1))
                   'implicit-\;))
        (setq arg (1- arg))))
  (while (< arg 0)
    (while (eq (swift-mode:token:type (swift-mode:backward-sexp-1))
               'implicit-\;))
    (setq arg (1+ arg))))

(defun swift-mode:forward-sexp-1 ()
  "Move forward a token or list.

Signal `scan-error' if it hits closing parentheses."
  (let ((token (swift-mode:forward-token-or-list))
        (pos (point)))
    (when (memq (swift-mode:token:type token) '(\] \) }))
      (goto-char pos)
      (signal 'scan-error
              (list "Unbalanced parentheses"
                    (swift-mode:token:start token)
                    (swift-mode:token:end token))))
    token))

(defun swift-mode:backward-sexp-1 ()
  "Move backward a token or list.

Signal `scan-error' if it hits opening parentheses."
  (let ((token (swift-mode:backward-token-or-list))
        (pos (point)))
    (when (memq (swift-mode:token:type token) '(\[ \( {))
      (goto-char pos)
      (signal 'scan-error
              (list "Unbalanced parentheses"
                    (swift-mode:token:start token)
                    (swift-mode:token:end token))))
    token))


;; Imenu

(defun swift-mode:mk-regex-for-def (keyword)
  "Make a regex matching the identifier introduced by KEYWORD."
  (concat "\\<" (regexp-quote keyword) "\\>"
          "\\s *"
          "\\("
          "\\(?:" "\\sw" "\\|" "\\s_" "\\)" "+"
          "\\)"))

(defconst swift-mode:imenu-generic-expression
  (list
   (list "Functions" (swift-mode:mk-regex-for-def "func") 1)
   (list "Classes"   (swift-mode:mk-regex-for-def "class") 1)
   (list "Enums"     (swift-mode:mk-regex-for-def "enum") 1)
   (list "Protocols" (swift-mode:mk-regex-for-def "protocol") 1)
   (list "Structs"   (swift-mode:mk-regex-for-def "struct") 1)
   (list "Extensions"   (swift-mode:mk-regex-for-def "extension") 1)
   (list "Constants" (swift-mode:mk-regex-for-def "let") 1)
   (list "Variables" (swift-mode:mk-regex-for-def "var") 1))
  "Value for `imenu-generic-expression' in `swift-mode'.")

;;;###autoload
(define-derived-mode swift-mode prog-mode "Swift"
  "Major mode for editing Swift code.

\\{swift-mode-map}"
  :syntax-table swift-mode:syntax-table
  :group 'swift

  (setq font-lock-defaults '(swift-mode:font-lock-keywords))

  (setq-local comment-start "// ")
  (setq-local comment-end "")
  ;; ":" is for Playground Rich Comments Markup Syntax:
  ;; https://developer.apple.com/library/prerelease/ios/documentation/Xcode/Reference/xcode_markup_formatting_ref/PlaygroundRichComments.html
  (setq-local comment-start-skip
              (concat
               "\\s *"
               "\\(?:"
               ;; Single-line comment
               "//+" ":?" "\\|"
               ;; Multi-line comment
               "/\\*+" ":?" "\\|"
               ;; Middle of multi-line-comment
               "\\*+ "
               "\\)"
               "\\s *"))
  (setq-local adaptive-fill-regexp comment-start-skip)
  (setq-local comment-multi-line t)

  (setq-local parse-sexp-lookup-properties t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'swift-mode:syntax-propertize-extend-region
            nil t)
  (setq-local syntax-propertize-function #'swift-mode:syntax-propertize)

  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'swift-mode:indent-line)

  (setq-local forward-sexp-function #'swift-mode:forward-sexp)

  (setq-local electric-indent-chars
              (append "{}()[]:;,." electric-indent-chars))

  (add-hook 'post-self-insert-hook #'swift-mode:post-self-insert nil t)

  (setq-local imenu-generic-expression swift-mode:imenu-generic-expression)

  (setq-local beginning-of-defun-function #'swift-mode:beginning-of-defun)
  (setq-local end-of-defun-function #'swift-mode:end-of-defun)

  (setq-local swift-mode:anchor-overlay
              (make-overlay (point-min) (point-min) nil t))

  (delete-overlay swift-mode:anchor-overlay)

  (add-hook 'which-func-functions #'swift-mode:current-defun-name)
  (setq-local add-log-current-defun-function #'swift-mode:current-defun-name))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

(provide 'swift-mode)

;;; swift-mode.el ends here
