(add-to-list 'load-path (concat zane-emacs-root "lib"))
(require 'rainbow-delimiters)
(eval-after-load "rainbow-delimiters"
  '(progn
     (setq-default frame-background-mode 'dark)
     (let ((supported-modes '(python-mode-hook
                              lisp-mode-hook
                              clojure-mode-hook
                              javascript-mode-hook)))
       (dolist (hook supported-modes)
         (add-hook hook '(lambda () (rainbow-delimiters-mode 1)))))))
