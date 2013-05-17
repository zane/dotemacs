(dolist (mode '(ruby-mode-hook
                js-mode-hook))
  (add-hook mode 'subword-mode))
