(defun z:reset-ret ()
  (ergoemacs-local-set-key (kbd "RET") 'reindent-then-newline-and-indent))

(eval-after-load "ergoemacs-mode"
  '(add-hook 'ruby-mode-hook 'z:reset-ret))

(add-to-list 'auto-mode-alist `("Gemfile" . ruby-mode))
