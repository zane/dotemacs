(eval-after-load
    "ergoemacs-mode"
    '(progn
       (ergoemacs-global-set-key (kbd "M-'") 'ace-jump-mode)
       (ergoemacs-global-set-key (kbd "C-'") 'comment-dwim)))