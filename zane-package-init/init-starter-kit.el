(after 'starter-kit
  (add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

  (after 'paredit
    (define-key paredit-mode-map (kbd "M-)") 'paredit-close-round-and-newline)
    (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
    (define-key paredit-mode-map (kbd "M-]") 'paredit-close-square-and-newline)
    (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
    (define-key paredit-mode-map (kbd "M-}") 'paredit-close-curly-and-newline)
    (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)))

(after 'starter-kit-defuns
  (defun turn-on-line-truncation ()
    "Sets `truncate-lines' to true"
    (setq truncate-lines t))
  (add-hook 'prog-mode-hook 'turn-on-line-truncation)

  (remove-hook 'text-mode-hook 'turn-on-auto-fill)
  (remove-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
  (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode))
