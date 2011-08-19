;;; zane-keys.el --- Keyboard shortcuts.
;;;
;;; http://www.gnu.org/software/emacs/elisp/html_node/Key-Binding-Conventions.html

(require 'cl)

(global-set-key (kbd "C-c C-a") 'align-regexp)
(global-set-key (kbd "C-c C-o") 'sort-lines)

(provide 'zane-keys)

;; foo [ bar
;; bazqux [ bar
;; splat [ bar
