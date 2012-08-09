;; http://xahlee.blogspot.com/2009/08/how-to-use-and-setup-emacss-whitespace.html
(setq whitespace-style '(face trailing tabs empty))
(add-hook 'before-save-hook 'whitespace-cleanup)
(global-whitespace-mode 1)
