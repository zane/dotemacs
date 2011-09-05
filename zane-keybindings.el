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

(setq mac-command-modifier 'meta)
;; (setq mac-function-modifier 'super)

(global-set-key (kbd "C-c C-a") 'align-regexp)
(global-set-key (kbd "C-c C-o") 'sort-lines)
(global-set-key (kbd "C-x y") 'bury-buffer)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen) ; http://www.stratospark.com/blog/2010/fullscreen_emacs_on_osx.html
;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)

(provide 'zane-keybindings)
