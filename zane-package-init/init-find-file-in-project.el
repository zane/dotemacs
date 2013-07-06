(after 'find-file-in-project
  (add-to-list 'ffip-patterns "*.erb")
  (add-to-list 'ffip-patterns "*.yml")
  (add-to-list 'ffip-patterns "*.css")
  (setq ffip-find-options "-not -regex \".*cache.*\"")

  )
(after "find-file-in-project-autoloads"
  (defun z/ffip-or-find-file ()
    "Try to ffip, but fall back to regular find file if not in the context of a project."
    (interactive)
    (condition-case ex
        (find-file-in-project)
      ('error (ido-find-file)))))
