(after 'python
  (setq python-python-command "ipython")
  ;; We never want to edit Python bytecode
  (add-to-list 'completion-ignored-extensions ".rbc"))
