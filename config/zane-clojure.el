(eval-after-load "clojure"
  '(progn
     (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)))
