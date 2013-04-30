(eval-after-load 'find-file-in-project
  '(progn
     (add-to-list 'ffip-patterns "*.erb")
     (add-to-list 'ffip-patterns "*.yml")
     (setq ffip-find-options "-not -regex \".*cache.*\"")

     (defun z/ffip-or-find-file ()
       "Try to ffip, but fall back to regular find file if not in the context of a project."
       (interactive)
       (condition-case ex
           (find-file-in-project)
         ('error (ido-find-file))))

     (eval-after-load "ergoemacs-mode"
       '(ergoemacs-global-set-key (kbd "C-o") 'z/ffip-or-find-file))))
