;;; zane-keys.el --- Keyboard shortcuts.
;;;
;;; http://www.gnu.org/software/emacs/elisp/html_node/Key-Binding-Conventions.html

(setq mac-command-modifier 'meta)
(setq mac-function-modifier 'super)

;; Navigation

(global-set-key (kbd "C-c C-a") 'align-regexp)
(global-set-key (kbd "C-c C-o") 'sort-lines)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-x y") 'bury-buffer)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen) ; http://www.stratospark.com/blog/2010/fullscreen_emacs_on_osx.html
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)

(global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-l") 'forward-char)  ; was downcase-word
(global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-k") 'next-line) ; was kill-sentence

(global-set-key (kbd "M-J") 'backward-sexp)
(global-set-key (kbd "M-L") 'forward-sexp)
(global-set-key (kbd "M-I") 'backward-up-list)
(global-set-key (kbd "M-K") 'down-list)

(global-set-key (kbd "s-j") 'windmove-left)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-i") 'windmove-up)
(global-set-key (kbd "s-k") 'windmove-down)

(global-set-key (kbd "C-j") 'backward-word)
(global-set-key (kbd "C-l") 'forward-word)

;; Unbinding of old navigation

(global-set-key (kbd))

(provide 'zane-keys)
