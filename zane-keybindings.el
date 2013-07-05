;;; zane-keys.el --- Keyboard shortcuts.
;;;
;;; http://www.masteringemacs.org/articles/2011/02/08/mastering-key-bindings-emacs/
;;; http://www.gnu.org/software/emacs/elisp/html_node/Key-Binding-Conventions.html
;;; http://code.google.com/p/ergoemacs/wiki/adoption

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)

;; Tab key
;; http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
(keyboard-translate ?\C-i ?\H-i)

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
  `(let ((function-symbol (global-key-binding ,key)))
     (when function-symbol
       (puthash ,key function-symbol old-key-bindings)
       (global-set-key (kbd ,key) (lambda() (interactive) (show-hint-old-kbind ,key))))))

(global-unset-key (kbd "C-x C-k"))

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-w") 'close-current-buffer)

(global-set-key (kbd "C-c C-a") 'align-regexp)
;(global-set-key (kbd "C-c C-o") 'sort-lines)
(global-set-key (kbd "C-x y") 'bury-buffer)
;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)

;; Unset M-SPC because it's used by Alfred.app
(global-unset-key (kbd "M-SPC"))
(global-unset-key (kbd "M-TAB"))

(global-unset-key (kbd "C-x RET"))
(add-hook 'term-exec-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix
                                              'utf-8-unix)))

;; Occur
;; http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(after 'expand-region-autoloads 
  (global-set-key (kbd "M->") 'er/expand-region)
  (global-set-key (kbd "M-<") 'er/contract-region))

(after 'ergoemacs-mode
  (after 'ace-jump-mode-autoloads 
    (ergoemacs-global-set-key (kbd "M-'") 'ace-jump-mode)
    (ergoemacs-global-set-key (kbd "C-'") 'comment-dwim))
  
  (after 'multiple-cursors-autoloads
    (ergoemacs-global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (ergoemacs-global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (ergoemacs-global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (ergoemacs-global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

  (after 'windmove
    (ergoemacs-global-set-key (kbd "H-i") 'windmove-up)
    (ergoemacs-global-set-key (kbd "C-l") 'windmove-right)
    (ergoemacs-global-set-key (kbd "C-j") 'windmove-left)
    (ergoemacs-global-set-key (kbd "C-k") 'windmove-down)))

(after "magit-autoloads"
  (global-set-key (kbd "C-c C-g") 'magit-status)
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

(after "key-chord-autoloads"
  (key-chord-mode 1))

(after "ace-jump-buffer-autoloads"
  (after "key-chord-autoloads"
    (key-chord-define-global "jk" 'ace-jump-buffer)))

(after 'dired 
  (define-key dired-mode-map (kbd "C-o") 'other-window))

(after 'smex-autoloads
  (global-set-key (kbd "M-a") 'smex)
  (global-set-key (kbd "M-A") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-a") 'execute-extended-command))

(provide 'zane-keybindings)
