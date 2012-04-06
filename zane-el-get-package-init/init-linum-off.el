(dolist (mode '(fundamental-mode
                magit-mode))
  (add-to-list 'linum-disabled-modes-list mode))
