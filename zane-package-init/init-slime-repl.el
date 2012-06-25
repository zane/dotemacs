(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (require 'clojure-mode)
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))

(eval-after-load 'slime
  (add-hook 'slime-repl-mode-hook 'turn-on-paredit-mode))
