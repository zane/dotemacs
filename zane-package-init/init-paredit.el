(autoload 'enable-paredit-mode "paredit")

(after 'paredit-autoloads
  (after 'clojure-mode-autoloads (add-hook 'clojure-mode-hook 'enable-paredit-mode))
  (after 'slime-autoloads (add-hook 'slime-repl-mode-hook 'enable-paredit-mode))

  (dolist (hook '(emacs-lisp-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'enable-paredit-mode)))

(after 'paredit
  ;; making paredit work with delete-selection-mode
  ;; https://github.com/malkomalko/.emacs.d/blob/master/modules/setup-paredit.el
  (put 'paredit-forward-delete 'delete-selection 'supersede)
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-open-round 'delete-selection t)
  (put 'paredit-open-square 'delete-selection t)
  (put 'paredit-doublequote 'delete-selection t)
  (put 'paredit-newline 'delete-selection t)

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
    (dolist (key (list (kbd "M-;")      ; searching
                       (kbd "C-d")      ; was paredit-forward-delete
                       (kbd "C-d")      ; was paredit-backward-delete
                       (kbd "M-d")      ; was paredit-backward-kill-word
                       (kbd "M-d")      ; was paredit-forward-kill-word
                       (kbd "C-k")      ; was paredit-kill
                       (kbd "M-J")))    ; moving to the beginning of the file
      (define-key paredit-mode-map key nil))

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
