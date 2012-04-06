(setq ac-auto-start nil)
(setq ac-sources (append ac-sources '(ac-source-yasnippet)))
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "M-i") 'ac-previous)
(define-key ac-menu-map (kbd "M-k") 'ac-next)

;; Auto-complete for text-files
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'text-mode-hook
             (lambda () (setq ac-sources '(ac-source-words-in-same-mode-buffers
                                      ac-source-files-in-current-dir
                                      ac-source-yasnippet))))
