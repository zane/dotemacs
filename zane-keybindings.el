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

(global-unset-key (kbd "C-x C-k"))

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

(after "expand-region-autoloads"
  (global-set-key (kbd "M->") 'er/expand-region)
  (global-set-key (kbd "M-<") 'er/contract-region))

(after 'ergoemacs-mode
  ;;(global-set-key (kbd "M-x") 'ergoemacs-cut-line-or-region)
  
  (after "ace-jump-mode-autoloads"
    (global-set-key (kbd "M-'") 'ace-jump-mode)
    (global-set-key (kbd "C-'") 'comment-dwim))

  (after "multiple-cursors-autoloads"
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

  (after 'windmove
    (global-set-key (kbd "H-i") 'windmove-up)
    (global-set-key (kbd "C-l") 'windmove-right)
    (global-set-key (kbd "C-j") 'windmove-left)
    (global-set-key (kbd "C-k") 'windmove-down))

  (after "find-file-in-project-autoloads"
    (global-set-key (kbd "C-o") 'z/ffip-or-find-file))

  (after 'edit-server
    (add-hook 'edit-server-edit-mode-hook
              (lambda ()
                (local-set-key (kbd "C-s" 'edit-server-done))))))

(after "magit-autoloads"
  (global-set-key (kbd "C-c C-g") 'magit-status)
  (after 'magit
    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

(after "key-chord-autoloads"
  (key-chord-mode 1))

(after "key-chord-autoloads"
  (after "ace-jump-buffer-autoloads"
    (key-chord-define-global "jk" 'ace-jump-buffer)))

(after 'dired
  (define-key dired-mode-map (kbd "C-o") 'other-window)
  (after 'dired-search-autoloads
    (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
    (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
    (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
    (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)))

(after 'smex-autoloads
  (global-set-key (kbd "M-a") 'smex)
  (global-set-key (kbd "M-A") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-a") 'execute-extended-command))

(provide 'zane-keybindings)
