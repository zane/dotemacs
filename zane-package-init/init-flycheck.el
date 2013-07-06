(after 'flycheck-autoloads
  ;; Use flycheck for all modes that aren't emacs-lisp-mode
  (add-hook 'find-file-hook
            (lambda ()
              (when (not (equal 'emacs-lisp-mode major-mode))
                (flycheck-mode)))))

(after 'flycheck  
  (set-face-attribute 'flycheck-error-face nil :underline "red")
  (set-face-attribute 'flycheck-warning-face nil :underline "yellow")
  (setq flycheck-mode-line-lighter " Κ"))
