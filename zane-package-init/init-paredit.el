(autoload 'enable-paredit-mode "paredit")

(dolist (hook '(emacs-lisp-mode-hook
                scheme-mode-hook))
  (add-hook hook 'enable-paredit-mode))

(eval-after-load 'clojure-mode '(add-hook 'clojure-mode-hook 'enable-paredit-mode))
(eval-after-load 'slime '(add-hook 'slime-repl-mode-hook 'enable-paredit-mode))

(dolist (hook '(ruby-mode-hook
                espresso-mode-hook
                text-mode-hook
                python-mode-hook))
  (add-hook hook 'esk-paredit-nonlisp))

(eval-after-load "paredit"
  '(progn
     (eval-after-load "windmove"
       '(progn
          (define-key paredit-mode-map (kbd "C-j") nil) ; windmove-left
          (define-key paredit-mode-map (kbd "C-k") nil) ; windmove-down
          ))

     (eval-after-load "ergoemacs-mode"
       '(progn
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
                      ))))))
