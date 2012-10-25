(add-hook 'ruby-mode-hook
          (lambda ()
            (eval-after-load "ergoemacs-mode"
              '(ergoemacs-local-set-key (kbd "RET") 'reindent-then-newline-and-indent))))
