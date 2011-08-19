(add-to-list 'load-path (concat zane-emacs-root "lib"))
(eval-after-load "rainbow-delimiters"
  '(progn
     (setq-default frame-background-mode 'dark)
     (let ((supported-modes '(emacs-lisp-mode
                              clojure-mode-hook
                              javascript-mode-hook
                              lisp-mode-hook
                              python-mode-hook)))
       (dolist (hook supported-modes)
         (add-hook hook 'zane-turn-on-rainbow-delimiters-mode)))))

(defun zane-turn-on-rainbow-delimiters-mode ()
  (rainbow-delimiters-mode 1))

(require 'rainbow-delimiters)
