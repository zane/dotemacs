(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-flyspell)

(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(remove-hook 'prog-mode-hook 'idle-highlight-mode)

(cua-mode nil)

(eval-after-load 'paredit
  ;; undo esk's rebindings -- ick!
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-close-round-and-newline)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-wrap-round)
     (define-key paredit-mode-map (kbd "M-]") 'paredit-close-square-and-newline)
     (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
     (define-key paredit-mode-map (kbd "M-}") 'paredit-close-curly-and-newline)
     (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)))
