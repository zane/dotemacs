(eval-after-load 'python-mode
  '(progn
     ;; We never watn to edit Python bytecode
     (add-to-list 'completion-ignored-extensions ".rbc")))
