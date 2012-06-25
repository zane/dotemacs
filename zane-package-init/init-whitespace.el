(setq whitespace-style '(face trailing tabs empty))
(add-hook 'before-save-hook 'whitespace-cleanup)
(global-whitespace-mode 1)
