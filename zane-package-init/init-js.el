(after 'js
  ;;(define-key js-mode-map "{" 'paredit-open-curly)
  ;;(define-key js-mode-map "}" 'paredit-close-curly-and-newline)
  (setq js-indent-level 2)
  (define-key js-mode-map (kbd ",") 'self-insert-command)
  (define-key js-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (font-lock-add-keywords 'js-mode `(("\\(function *\\)("
                                      (0 (progn (compose-region (match-beginning 1)
                                                                (match-end 1)
                                                                "ƒ")
                                                nil))))))
