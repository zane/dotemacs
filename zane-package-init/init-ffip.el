(eval-after-load "find-file-in-project"
  '(progn (add-to-list 'ffip-patterns "*.erb")
          (setq ffip-find-options "-not -regex \".*cache.*\"")))
