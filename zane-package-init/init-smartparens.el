(after "smartparens-autoloads"
  (setq sp-base-key-bindings 'paredit)
  
  (defun turn-on-smartparens-mode ()
    "Turn on `smartparens-mode'."
    (smartparens-mode +1))

  (dolist (mode-hook '(ruby-mode-hook
                       python-mode-hook
                       js-mode-hook))
    (add-hook mode-hook 'turn-on-smartparens-mode)))
