;;; init-theme.el ---  setup theme. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;
;; Usage
;; item of time-themes-table: (hours-in-string . theme-name)
;; you could add more items.
;; refer link: https://emacs-china.org/t/topic/1348

;;; Code:

;;timer for automatically changing themes
(setq kevin-interval-timer nil)

;;table is used to save (time themes) pair for automatically changing themes
;;time should be a string. themes should be a variant , not symbos.
(setq kevin-themes-table nil)

;; current theme name
(setq kevin-current-theme nil)

(defun kevin/init-themes-table (tt)
  "Set themes table."
  (setq kevin-themes-table
        ;; sort firstly, get-themes-according require a sorted table.
        (sort tt (lambda (x y) (< (string-to-number (car x)) (string-to-number (car y)))))))


;; ====================================Themes automatically change =====================================
(defun kevin/get-themes-according-time (hour-string)
  "This function return the theme according to hour-string.
Value of hour-string should be between 1 and 24(including)."
  (catch 'break
    (let (
          (now-time (string-to-number hour-string))
          ;; init current-themes to the themes of final item
          (correct-themes (cdr (car (last kevin-themes-table))))
          (loop-list kevin-themes-table)
          )

      ;; loop to set correct themes to correct-themes
      (while loop-list
        (let ((v (car loop-list)))
          (let ((v-time (string-to-number (car v))) (v-themes (cdr v)))
            (if (< now-time v-time)
                (throw 'break correct-themes)  ; t
              (setq correct-themes v-themes) ; nil
              )))
        (setq loop-list (cdr loop-list))
        )
      ;; This is returned for value of hour-string is bigger than or equal to car of final item
      (throw 'break correct-themes) ; t
      ))
  )

(defun kevin/check-time-and-modify-theme ()
  "This function will get the theme of now according to time-table-themes,
then check whether emacs should to modify theme, if so, modify it."
  (let ((new-theme (kevin/get-themes-according-time (format-time-string "%H"))))
    (unless (eq new-theme kevin-current-theme)
      (load-theme new-theme t)
      (setq kevin-current-theme new-theme)
      ))
  )

(defun kevin/start-themes-auto-change ()
  "Start to automatically change themes."
  (interactive)
  (kevin/check-time-and-modify-theme)
  (setq
   kevin--interval-timer (run-at-time 3600 3600 'mp-ui/check-time-and-modify-theme))
  (message "themes auto change open."))

(defun kevin/stop-themes-auto-change ()
  "Close automatically change themes."
  (interactive)
  (cancel-timer kevin-interval-timer)
  (message "themes auto change close."))


;; 6:00 - 17::00 use zenburn, 17:00 - 24:00 use tomorrow, 24:00 - 6:00 use zenburn
(kevin/init-themes-table '(("6" . zenburn) ("17" . doom-tomorrow-night)))

(kevin/start-themes-auto-change)


(provide 'init-theme)
