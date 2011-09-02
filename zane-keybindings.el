;;; zane-keys.el --- Keyboard shortcuts.
;;;
;;; http://www.gnu.org/software/emacs/elisp/html_node/Key-Binding-Conventions.html

;;; http://code.google.com/p/ergoemacs/wiki/adoption

;; hash table to store the old key bindings
(setq old-key-bindings (make-hash-table :test 'equal))

;; shows a hint about the change of the key binding
(defun show-hint-old-kbind (key)
  (let ((function-symbol (gethash key old-key-bindings)))
    (beep)
    (message "You typed: %s. For %s, use %s."
             key
             function-symbol
             (mapcar 'key-description (where-is-internal function-symbol)))))

;; turns off a key binding, leaving a hint for the unbound command
(defmacro global-unset-key-leave-hint (key)
  `(let ((function-symbol (global-key-binding (kbd ,key))))
     (when function-symbol
       (puthash ,key function-symbol old-key-bindings)
       (global-set-key (kbd ,key) (lambda() (interactive) (show-hint-old-kbind ,key))))))

;; Move down
(global-set-key (kbd "M-k") 'next-line)
(global-unset-key-leave-hint "<down>")
(global-unset-key-leave-hint "C-n")

;; Move up
(global-set-key (kbd "M-i") 'previous-line)
(global-unset-key-leave-hint "<up>")
(global-unset-key-leave-hint "C-p")

;; Move left
(global-set-key (kbd "M-j") 'backward-char)
(global-unset-key-leave-hint "<left>")
(global-unset-key-leave-hint "C-b")

;; Move right
(global-set-key (kbd "M-l") 'forward-char)
(global-unset-key-leave-hint "<right>")
(global-unset-key-leave-hint "C-f")

;; Cut
(global-unset-key-leave-hint "C-w")
(global-set-key (kbd "M-x") 'kill-region)

;; Copy
(global-unset-key-leave-hint "M-w")
(global-set-key (kbd "M-c") 'kill-ring-save)

;; Paste
(global-unset-key-leave-hint "M-y")
(global-set-key (kbd "M-v") 'yank)
(global-unset-key-leave-hint "C-y")

;; Undo
(global-set-key (kbd "M-z") 'undo)
(global-unset-key-leave-hint "C-_")

;; Move by word
(global-unset-key-leave-hint "M-b")
(global-set-key (kbd "M-u") 'backward-word)
(global-unset-key-leave-hint "M-f")
(global-set-key (kbd "M-o") 'forward-word)

;; Move to beginning/ending of line
(global-unset-key-leave-hint "C-a")
(global-set-key (kbd "M-h") 'move-beginning-of-line)
(global-unset-key-leave-hint "C-e")
(global-set-key (kbd "M-H") 'move-end-of-line)

;; Fast movement
(global-set-key (kbd "M-I") 'scroll-down-command)
(global-set-key (kbd "M-K") 'scroll-up-command)

;; Execute Extended Command
(global-set-key (kbd "M-a") 'execute-extended-command)

;; Other

(global-set-key (kbd "M-~") 'switch-to-previous-frame)
(global-set-key (kbd "M-`") 'switch-to-next-frame)

;; TODO: Redo

(setq mac-command-modifier 'meta)
;; (setq mac-function-modifier 'super)

;; Navigation

(global-set-key (kbd "C-c C-a") 'align-regexp)
(global-set-key (kbd "C-c C-o") 'sort-lines)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-x y") 'bury-buffer)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen) ; http://www.stratospark.com/blog/2010/fullscreen_emacs_on_osx.html
;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)

;; (global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
;; (global-set-key (kbd "M-l") 'forward-char)  ; was downcase-word
;; (global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
;; (global-set-key (kbd "M-k") 'next-line) ; was kill-sentence

;; (global-set-key (kbd "M-J") 'backward-sexp)
;; (global-set-key (kbd "M-L") 'forward-sexp)
;; (global-set-key (kbd "M-I") 'backward-up-list)
;; (global-set-key (kbd "M-K") 'down-list)

;; (global-set-key (kbd "s-j") 'windmove-left)
;; (global-set-key (kbd "s-l") 'windmove-right)
;; (global-set-key (kbd "s-i") 'windmove-up)
;; (global-set-key (kbd "s-k") 'windmove-down)

;; (global-set-key (kbd "C-j") 'backward-word)
;; (global-set-key (kbd "C-l") 'forward-word)

;; Unbinding of old navigation

;; (global-set-key (kbd "C-n") nil)
;; (global-set-key (kbd "C-n") nil)
;; (global-set-key (kbd "M-f") nil)
;; (global-set-key (kbd "M-b") nil)

(provide 'zane-keybindings)
