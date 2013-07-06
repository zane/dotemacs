(after 'clojure-mode-autoloads
  (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup))

(after 'coffee-mode
  (defun z:set-tab-with-to-2 ()
    (set (make-local-variable 'tab-width) 2))
  (add-hook 'coffee-mode-hook 'z:set-tab-with-to-2))

(after 'ruby-mode
  (after 'ergoemacs-mode
    (defun z:reset-ret ()
      (ergoemacs-local-set-key (kbd "RET") 'reindent-then-newline-and-indent))
    (add-hook 'ruby-mode-hook 'z:reset-ret)))
