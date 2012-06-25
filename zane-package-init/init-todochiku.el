(defun my-growl-matched (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (todochiku-message "ERC Mention"
     (concat (buffer-name (current-buffer)) ": "
             message)
     (todochiku-icon 'irc))))

(add-hook 'erc-text-matched-hook 'my-growl-matched)


