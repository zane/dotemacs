(after 'multiple-cursors-autoloads
  (after 'ergoemacs-mode
    (ergoemacs-global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (ergoemacs-global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (ergoemacs-global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (ergoemacs-global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

  (dolist (command '(paredit-backward-delete
                     paredit-forward-delete))
    (add-to-list 'mc/cmds-to-run-for-all command)))
