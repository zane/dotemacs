(autoload 'enable-paredit-mode "paredit")

(after 'paredit-autoloads
  (after 'clojure-mode-autoloads (add-hook 'clojure-mode-hook 'enable-paredit-mode))
  (after 'slime-autoloads (add-hook 'slime-repl-mode-hook 'enable-paredit-mode))

  (dolist (hook '(emacs-lisp-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'enable-paredit-mode))


(after 'paredit
  (defadvice yank
    (after yank-indent-pp-sexp activate)
    (save-excursion
      (if paredit-mode
          (condition-case ex
              (progn
                (paredit-backward-up)
                (indent-pp-sexp))
            ('error nil)))))

    (after 'ergoemacs-mode
    (define-key paredit-mode-map (kbd "C-d") nil) ; was paredit-forward-delete
    (define-key paredit-mode-map (kbd "C-d") nil) ; was paredit-backward-delete
    (define-key paredit-mode-map (kbd "M-d") nil) ; was paredit-backward-kill-word
    (define-key paredit-mode-map (kbd "M-d") nil) ; was paredit-forward-kill-word
    (define-key paredit-mode-map (kbd "C-k") nil) ; was paredit-kill

    (define-key paredit-mode-map (kbd "M-;") nil) ; searching
    (define-key paredit-mode-map (kbd "M-J") nil) ; moving to the beginning of the file

    (add-hook 'paredit-mode-hook
              (lambda ()
                (ergoemacs-local-set-key (kbd "M-f") 'paredit-forward-delete)
                (ergoemacs-local-set-key (kbd "M-d") 'paredit-backward-delete)
                (ergoemacs-local-set-key (kbd "M-e") 'paredit-backward-kill-word)
                (ergoemacs-local-set-key (kbd "M-r") 'paredit-forward-kill-word)
                (ergoemacs-local-set-key (kbd "M-g") 'paredit-kill)
                )))

  (after 'windmove
    ;; windmove-left
    (define-key paredit-mode-map (kbd "C-j") nil)
    ;; windmove-down
    (define-key paredit-mode-map (kbd "C-k") nil)))
)
