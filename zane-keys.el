;;; zane-keys.el --- Keyboard shortcuts.
;;;
;;; http://www.gnu.org/software/emacs/elisp/html_node/Key-Binding-Conventions.html

(setq mac-command-modifier 'meta)
(global-set-key (kbd "C-c C-a") 'align-regexp)
(global-set-key (kbd "C-c C-o") 'sort-lines)
(global-set-key (kbd "C-x y") 'bury-buffer)
(global-set-key (kbd "C-o")  'other-window)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen) ; http://www.stratospark.com/blog/2010/fullscreen_emacs_on_osx.html
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)

(provide 'zane-keys)
