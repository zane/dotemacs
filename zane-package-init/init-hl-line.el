(after 'hl-line
  (dolist (hook '(dired-mode-hook
                  package-menu-mode-hook
                  buffer-menu-mode-hook))
    (add-hook hook 'hl-line-mode)))
