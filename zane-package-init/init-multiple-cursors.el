(eval-after-load
    "ergoemacs-mode"
  '(progn
     (ergoemacs-global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
     (ergoemacs-global-set-key (kbd "C->") 'mc/mark-next-like-this)
     (ergoemacs-global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
     (ergoemacs-global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

(defvar z:cmds-to-run-for-all
  '(
    paredit-backward-delete
    paredit-forward-delete
    ))

(eval-after-load
    "multiple-cursors"
  '(dolist (command z:cmds-to-run-for-all)
     (add-to-list 'mc/cmds-to-run-for-all command)))
