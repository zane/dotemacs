(eval-after-load 'rcirc
  '(progn
     (add-to-list 'load-path (concat zane-emacs-root "lib"))
     (require 'rcirc-notify)
     (add-to-list 'rcirc-server-alist
                  '("yamato.obama.local"
                    :nick "zane"
                    :channels ("#tech"
                               "#ofa"
                               "#og-tech")))
     (add-to-list 'rcirc-server-alist
                  '("irc.freenode.net"
                    :nick "zanes"
                    :channels ("#emacs"
                               "#node.js"
                               "#scheme"
                               "#javascript"
                               "#pocoo")))
     (add-to-list 'rcirc-server-alist
                  '("irc.freenode.net"
                    :nick "zanes"
                    :channels ("#emacs"
                               "#node.js"
                               "#scheme"
                               "#javascript"
                               "#pocoo")))
     (add-to-list 'rcirc-server-alist
                  '("irc.mozilla.org"
                    :nick "zane"
                    :channels ("#narcissus")))
     ;; (add-to-list 'rcirc-server-alist
     ;;              '("irc.prison.org"
     ;;                :nick "Vaeshir"
     ;;                :channels ("#avendar")))
     (setq rcirc-default-nick      "zane")
     (setq rcirc-default-user-name "zane")
     (setq rcirc-default-full-name "Zane Shelby")))
