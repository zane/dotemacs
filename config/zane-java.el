(add-hook 'java-mode-hook
          (lambda ()
            (subword-mode t)
            (setq tab-width 4)))
