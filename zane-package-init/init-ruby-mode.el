(after 'ruby-mode
  (after 'ergoemacs-mode
    (defun z:reset-ret ()
      (ergoemacs-local-set-key (kbd "RET") 'reindent-then-newline-and-indent))
    (add-hook 'ruby-mode-hook 'z:reset-ret)))

(add-to-list 'auto-mode-alist `("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
