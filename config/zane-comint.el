(setq explicit-shell-file-name "bash")
(add-hook 'comint-mode-hook
          (lambda ()
            (ergoemacs-local-set-key (kbd "C-r") 'comint-history-isearch-backward)))
