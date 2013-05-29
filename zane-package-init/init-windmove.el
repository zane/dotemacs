(require 'windmove)
(after 'windmove
  (after 'ergoemacs-mode
    (ergoemacs-global-set-key (kbd "H-i") 'windmove-up)
    (ergoemacs-global-set-key (kbd "C-l") 'windmove-right)
    (ergoemacs-global-set-key (kbd "C-j") 'windmove-left)
    (ergoemacs-global-set-key (kbd "C-k") 'windmove-down)))
