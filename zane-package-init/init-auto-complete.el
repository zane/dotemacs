(require 'auto-complete)

(after 'auto-complete-autoloads 
  (setq ac-auto-start nil)
  (setq ac-show-menu-immediately-on-auto-complete t)
  (setq ac-dwim t)
  (setq ac-delay 0)
  (setq ac-expand-on-auto-complete t)
  (ac-set-trigger-key "TAB")

  (setq ac-use-menu-map t)
  (setq ac-use-fuzzy nil)
  (after 'fuzzy (setq ac-use-fuzzy t))
  (define-key ac-menu-map (kbd "M-i") 'ac-previous)
  (define-key ac-menu-map (kbd "M-k") 'ac-next)
  (global-auto-complete-mode t))
