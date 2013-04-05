(eval-after-load 'js '(progn ;(define-key js-mode-map "{" 'paredit-open-curly)
                             ;(define-key js-mode-map "}" 'paredit-close-curly-and-newline)
                             (setq js-indent-level 2)
                             (define-key js-mode-map (kbd ",") 'self-insert-command)
                             (font-lock-add-keywords 'js-mode `(("\\(function *\\)("
                                                                 (0 (progn (compose-region (match-beginning 1)
                                                                                           (match-end 1)
                                                                                           "ƒ")
                                                                           nil)))))))
