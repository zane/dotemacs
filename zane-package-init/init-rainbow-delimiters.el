(after 'rainbow-delimiters-autoloads
  (defun zane-turn-on-rainbow-delimiters-mode ()
    (interactive)
    (rainbow-delimiters-mode 1))

  (setq-default frame-background-mode 'dark)
  (let ((hooks '(emacs-lisp-mode-hook
                 clojure-mode-hook
                 javascript-mode-hook
                 lisp-mode-hook
                 python-mode-hook)))
    (dolist (hook hooks)
      (add-hook hook 'zane-turn-on-rainbow-delimiters-mode))))
