(global-set-key (kbd "C-c C-g") 'magit-status)

(after 'magit
  (setq magit-status-buffer-switch-function 'switch-to-buffer)

  ;; Make magit restore the original window configuration when you leave the
  ;; magit buffer.
  ;;
  ;; http://whattheemacsd.com/setup-magit.el-01.html

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))
