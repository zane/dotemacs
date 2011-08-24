(add-to-list 'load-path (concat user-emacs-directory "lib"))
(eval-after-load "dired"
  '(progn
     (require 'dired+)
     (require 'image-dired)

     (add-to-list 'font-lock-maximum-decoration '(dired-mode . nil))

     (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
     (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
     (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
     (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)
     (define-key dired-mode-map (kbd "C-o") 'other-window)

     ;; Refresh the dired buffer whenever it regains focus
     (setq-default dired-auto-revert-buffer t)))
