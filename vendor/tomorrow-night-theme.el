;;; tomorrow-night-theme.el --- custom theme for faces

;;; Commentary:
;;
;;; Tomorrow Night Theme
;;
;; My customized version of Chris Kempson's Tomorrow Night Bright theme:
;; https://github.com/ChrisKempson/Tomorrow-Theme
;;
;;; Code:

(deftheme tomorrow-night
  "A Pastel Coloured Theme")

(let ((background "#1d1f21")
      (selection "#373b41")
      (foreground "#c5c8c6")
      (comment "#969896")
      (cursor "#949494")
      (gray "#303030")
      (gray-2 "#1c1c1c")
      (gray-3 "#121212")
      (gray-4 "#080808")
      (red "#d54e53")
      (red-2 "#cd0000")
      (red-3 "#5f0000")
      (yellow "#e7c547")
      (yellow-2 "#cdcd00")
      (orange "#e78700")
      (green "#b5bd68")
      (aqua "#8abeb7")
      (blue "#7aa6da")
      (purple "#c397d8")
      )

  (custom-theme-set-faces
   'tomorrow-night

   ;; Basics
   `(default ((t (:background ,background :foreground ,foreground))))
   `(region ((t (:background ,selection))))
   `(fringe ((t (:background ,gray-2))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(hl-line ((t (:background ,gray-3))))
   `(highlight ((t (:background ,gray-3))))
   `(lazy-highlight ((t (:background ,gray-3))))
   `(match ((t (:background ,background :foreground ,red))))
   `(isearch ((t (:background ,background :foreground ,red))))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((t (:foreground ,blue))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,green))))
   `(font-lock-doc-face ((t (:foreground ,comment))))
   `(font-lock-doc-string-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple))))
   `(font-lock-negation-char-face ((t (:foreground ,aqua))))
   `(font-lock-preprocessor-face ((t (:foreground ,red))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,aqua))))
   `(font-lock-warning-face ((t (:foreground ,red))))

   ;; UI related
   `(mode-line ((t (:background ,selection :foreground ,foreground))))
   `(mode-line-inactive ((t (:background ,gray-3))))
   `(vertical-border ((t (:background ,background :foreground ,selection))))

   ;; linum-mode
   `(linum ((t (:background ,background :foreground ,gray))))

   ;; flycheck
   `(flycheck-warning ((t (:underline (:color foreground-color :style wave)))))

   ;; show-paren-mode
   `(show-paren-match ((t (:background ,purple :foreground ,gray-2))))
   `(show-paren-mismatch ((t (:background ,orange :foreground ,gray-2))))

   ;; ido
   `(ido-only-match ((t (:foreground ,orange))))
   `(ido-subdir ((t (:foreground ,purple))))

   ;; whitespace-mode
   `(whitespace-empty ((t (:background ,yellow-2 :foreground ,red))))
   `(whitespace-hspace ((t (:foreground ,gray-2))))
   `(whitespace-indentation ((t (:foreground ,gray-2))))
   `(whitespace-line ((t (:background ,gray-2))))
   `(whitespace-newline ((t (:foreground ,gray-2))))
   `(whitespace-space ((t (:foreground ,gray-2))))
   `(whitespace-space-after-tab ((t (:foreground ,gray-2))))
   `(whitespace-tab ((t (:foreground ,gray-2))))
   `(whitespace-trailing ((t (:background ,red-3 :foreground ,yellow))))

   ;; flyspell-mode
   `(flyspell-incorrect ((t (:foreground ,orange :underline ,orange))))
   `(flyspell-duplicate ((t (:foreground ,orange :underline ,orange))))

   ;; magit
   `(magit-diff-add ((t (:foreground ,green))))
   `(magit-diff-del ((t (:foreground ,red))))
   `(magit-item-highlight ((t (:background ,gray-2))))

   ;; highlight-indentation-mode
   `(highlight-indentation-face ((t (:background ,gray-4))))
   `(highlight-indentation-current-column-face ((t (:background ,gray-3))))

   ;; diff-hl
   `(diff-hl-change ((t (:background "#3a81c3"))))
   `(diff-hl-delete ((t (:background "#ee6363"))))
   `(diff-hl-insert ((t (:background "#7ccd7c"))))

   ;; ECB
   `(ecb-default-highlight-face ((t (:background ,background :foreground ,red-2))))

   ;; org-mode
   `(org-date ((t (:foreground ,purple))))
   `(org-done ((t (:foreground ,green))))
   `(org-hide ((t (:foreground ,gray-2))))
   `(org-link ((t (:foreground ,blue))))
   `(org-todo ((t (:foreground ,red))))
   )

  (custom-theme-set-variables
   'tomorrow-night

   ;; Fill Column Indicator mode
   `(fci-rule-color ,gray-2)
   `(fci-rule-character-color ,gray-2)

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])))

(provide-theme 'tomorrow-night)

;;; tomorrow-night-theme.el ends here
