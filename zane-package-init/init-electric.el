(after 'electric
  ;; Seems to mess up js-mode
  ;; (electric-indent-mode +1)
  
  (defun turn-on-electric-pair-mode ()
    "Turn on `electric-pair-mode'."
    (electric-pair-mode +1))

  (dolist (mode-hook '(ruby-mode-hook
                       python-mode-hook
                       js-mode-hook))
    (add-hook mode-hook 'turn-on-electric-pair-mode)))
