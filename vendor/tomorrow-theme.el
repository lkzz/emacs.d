;;; tomorrow-night-theme.el --- custom theme for faces

;;; Commentary:
;;
;;; Tomorrow Night Theme
;;
;; My customized version of Chris Kempson's Tomorrow Night theme:
;; https://github.com/ChrisKempson/Tomorrow-Theme
;;
;;; Code:


(deftheme tomorrow
  "A dark version of tomorrow theme")

(let ((class '((class color) (min-colors 89)))
      (default "#abb2bf" )
      (light "#ccd4e3")
      (foreground "#C5C8C6")
      (background "#1d1f21")
      (background-dark "#1a1a1a")
      (background-darker "#000000")
      (mode-line-inactive "#1c2129")
      (mode-line-active "#6f337e")
      (background-lighter "#3a3f4b")
      (background-red "#4c3840")
      (bright-background-red "#744a5b")
      (background-purple "#48384c")
      (background-blue "#38394c")
      (bright-background-blue "#4e5079")
      (background-green "#3d4a41")
      (bright-background-green "#3f6d54")
      (background-orange "#4a473d")
      (hl-line "#282a2e")
      (grey "#303030")
      (grey-dark "#666666")
      (highlight "#3e4451")
      (comment "#969896")
      (orange "#DE935F")
      (orange-light "#ddbd78")
      (red "#CC6666")
      (yellow "#B5BD68")
      (purple "#B294BB")
      (purple-dark "#64446d")
      (blue "#81A2BE")
      (blue-dark "#1f5582")
      (green "#98be65")
      (green-light "#9eac8c")
      (peach "PeachPuff3")
      (aqua "#8ABEB7")
      (diff-added-background "#284437")
      (diff-added-refined-background "#1e8967")
      (diff-removed-background "#583333")
      (diff-removed-refined-background "#b33c49")
      (diff-current-background "#29457b")
      (diff-current-refined-background "#4174ae"))

  (custom-theme-set-faces
   'tomorrow
   ;; Basics
   `(default ((,class (:background ,background-dark :foreground ,foreground))))
   `(cursor ((,class (:background ,default))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,background-dark :foreground ,comment))))
   `(border ((,class (:foreground ,background-lighter))))
   `(vertical-border ((,class (:foreground ,background-lighter))))
   `(highlight ((,class (:background ,highlight :foreground ,default :underline nil))))
   `(region ((,class (:background ,highlight))))
   `(secondary-selection ((,class (:background ,highlight :foreground ,default))))
   `(isearch ((,class (:background ,orange-light :foreground ,highlight))))
   `(lazy-highlight ((,class (:background ,grey-dark :foreground ,orange-light))))
   `(hl-line ((,class (:background ,hl-line :underline unspecified :inherit nil))))
   `(shadow ((,class (:foreground ,comment))))
   `(match ((,class (:background ,background-green))))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((,class (:foreground ,blue))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,red))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-doc-string-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,purple))))
   `(font-lock-negation-char-face ((,class (:foreground ,aqua))))
   `(font-lock-preprocessor-face ((,class (:foreground ,red))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
   `(font-lock-string-face ((,class (:foreground ,yellow))))
   `(font-lock-type-face ((,class (:foreground ,yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,aqua))))
   `(font-lock-warning-face ((,class (:foreground ,red))))

   ;; error & success
   `(error ((,class (:foreground ,red :weight bold))))
   `(warning ((,class (:foreground ,orange :weight bold))))
   `(success ((,class (:foreground ,green :weight bold))))

   ;; Mode line faces
   `(mode-line ((,class (:foreground ,foreground :background ,hl-line :weight normal
                                     :box (:line-width 1 :color ,hl-line)))))
   `(mode-line-inactive ((,class (:inherit mode-line
                                           :foreground ,comment
                                           :background ,highlight :weight normal))))
   ;;   `(mode-line-emphasis (:foreground ,foreground :slant italic))
   ;;   `(mode-line-highlight (:foreground ,purple :box nil :weight bold))
   `(mode-line-highlight ((,class (:foreground ,purple :box nil :weight bold))))
   `(header-line ((,class (:inherit mode-line-inactive))))

   )

  )



(provide-theme 'tomorrow)
;;; tomorrow-theme.el ends here
