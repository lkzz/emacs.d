;;; init-email.el --- emacs mail client. -*- lexical-binding: t -*-
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;;; Commentary:
;;     1. brew install mu --with-emacs
;;     2. brew install offlineimap
;;; Code:

(use-package mu4e
  :ensure nil
  :defer t
  :config
  ;; Use mu4e as default mail agent
  (setq mail-user-agent 'mu4e-user-agent)
  ;; Mail folder set to ~/Maildir
  (setq mu4e-maildir "~/Workspace/mail")         ; NOTE: should not be symbolic link
  ;; Fetch mail by offlineimap
  (setq mu4e-get-mail-command "offlineimap")
  ;; Fetch mail in 60 sec interval
  (setq mu4e-update-interval 60)
  ;; folder for sent messages
  (setq mu4e-sent-folder   "/Sent Messages")
  ;; unfinished messages
  (setq mu4e-drafts-folder "/Drafts")
  ;; trashed messages
  (setq mu4e-trash-folder  "/Deleted Messages")
  ;; saved messages
  (setq mu4e-trash-folder  "/INBOX")


  ;; SMTP setup
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        starttls-use-gnutls t)
  ;; Personal info
  (setq user-full-name "kevin leung")
  (setq user-mail-address "309859548@qq.com")
  ;; stmp mail
  (setq smtpmail-smtp-server "smtp.qq.com")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-smtp-user "309859548@qq.com")
  (setq mu4e-compose-signature "sent from my emacs.")

  )

(use-package mu4e-contrib
  :ensure nil
  :init
  (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; try to emulate some of the eww key-bindings
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))
  (setq mu4e-view-show-images t)
  )


;; (use-package mu4e-alert
;;   :ensure t
;;   :after mu4e
;;   :init (with-eval-after-load 'mu4e
;;           (when mu4e-enable-notifications
;;             (mu4e-alert-enable-notifications))
;;           (when mu4e-enable-mode-line
;;             (mu4e-alert-enable-mode-line-display))))

;; (use-package mu4e-maildirs-extension
;;   :ensure t
;;   :after mu4e
;;   ;;;if mu4e-use-maildirs-extension
;;                                         ; :init (with-eval-after-load 'mu4e (mu4e-maildirs-extension-load))
;;   )

(provide 'init-email)
