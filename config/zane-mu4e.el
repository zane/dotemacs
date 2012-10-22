(let ((mu4e-dir "/usr/local/share/emacs/site-lisp/mu4e/"))
  (if (file-exists-p mu4e-dir)
      (progn (add-to-list 'load-path mu4e-dir)
             (require 'mu4e))))

(eval-after-load 'mu4e
  '(progn
     (setq mu4e-drafts-folder "/[Gmail].Drafts")
     (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
     (setq mu4e-trash-folder  "/[Gmail].Trash")

     ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
     (setq mu4e-sent-messages-behavior 'delete)

     (setq mu4e-maildir-shortcuts
           '(("/INBOX" . ?i)
             ("/[Gmail].All Mail" . ?a)
             ("/[Gmail].Sent Mail" . ?t)
             ("/[Gmail].Trash" . ?s)
             ("/bulk" . ?b)
             ("/note" . ?n)
             ))
     ;; allow for updating mail using 'U' in the main view:
     (setq mu4e-get-mail-command "mbsync gmail-inboxes")
     ;; something about ourselves
     (setq
      user-mail-address "zaneshelby@gmail.com"
      user-full-name "Zane Shelby"
      message-signature "")

     (require 'smtpmail)

     (setq message-send-mail-function 'smtpmail-send-it
           smtpmail-stream-type 'starttls
           smtpmail-default-smtp-server "smtp.gmail.com"
           smtpmail-smtp-server "smtp.gmail.com"
           smtpmail-smtp-service 587)

     ;; don't keep message buffers around
     (setq message-kill-buffer-on-exit t)

     (let ((downloads-dir (concat user-home-directory "Downloads")))
       (if (file-exists-p downloads-dir)
           (setq mu4e-attachment-dir downloads-dir)))

     (setq mu4e-mu-binary (executable-find "mu"))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ZANE CUSTOMIZATIONS
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (setq mu4e-maildir (concat user-home-directory ".mbsync/GMail"))

     (setq mu4e-view-prefer-html nil)
     (setq mu4e-html2text-command "html2text -utf8 -nobs -width 72")

     ;; enable inline images
     (setq mu4e-view-show-images t)
     ;; use imagemagick, if available
     (when (fboundp 'imagemagick-register-types)
       (imagemagick-register-types))

     ))
