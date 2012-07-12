(dolist (extension '("\\.mkd" "\\.md"))
  (add-to-list 'auto-mode-alist `(,extension . markdown-mode)))
