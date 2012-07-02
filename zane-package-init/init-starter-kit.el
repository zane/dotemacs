(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-flyspell)

(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(cua-mode nil)
