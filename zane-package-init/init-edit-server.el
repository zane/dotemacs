(require 'edit-server)
(after 'edit-server
  (edit-server-start)

  (add-hook 'mail-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("^[ \t]*>[ \t]*>[ \t]*>.*$"
                  (0 'mail-multiply-quoted-text-face))
                 ("^[ \t]*>[ \t]*>.*$"
                  (0 'mail-double-quoted-text-face))))))

  (setq edit-server-new-frame nil))
