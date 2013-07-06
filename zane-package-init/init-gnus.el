(after 'gnus
  (setq gnus-select-method '(nnimap "gmail"
                                    (nnimap-address "imap.gmail.com")
                                    (nnimap-server-port 993)
                                    (nnimap-stream ssl)))

  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "zaneshelby@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)

  (setq gnus-posting-styles
        '((".*"
           (name "Zane Shelby")
           ("X-URL" "http://www.zaneshelby.com"))))

  (setq user-full-name "Zane Shelby")
  (setq user-mail-address "zaneshelby@gmail.com")
  (setq send-mail-function 'smtpmail-send-it))
