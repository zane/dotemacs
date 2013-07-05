(after 'multiple-cursors
    (dolist (command '(paredit-backward-delete
                       paredit-forward-delete))
      (add-to-list 'mc/cmds-to-run-for-all command)))
