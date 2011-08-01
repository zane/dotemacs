(add-to-list 'load-path (concat dotfiles-dir "/lib/coffee-mode.github.defunkt"))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(autoload 'coffee-mode "coffee-mode")
(eval-after-load "coffee-mode"
  '(progn
     (add-hook 'coffee-mode-hook
               '(lambda () (set (make-local-variable 'tab-width) 2)))))
