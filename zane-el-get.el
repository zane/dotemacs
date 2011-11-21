(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

;; Get el-get and install it if we don't have it already.
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(require 'el-get)

;; Since this is used in multiple places:
(setq org-directory (expand-file-name (concat user-dropbox-directory
                                              "/Documents/deft")))

;; Add custom package.el sources (mainly for starter-kit)
(add-to-list 'package-archives '("technomancy" . "http://repo.technomancy.us/emacs/") t)
(package-initialize)

(setq el-get-sources
      '((:name auto-complete
               :after (lambda ()
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
                                                              ac-source-yasnippet))))))
        (:name color-theme-solarized
               :features solarized-dark-theme
               :after (lambda ()
                        (enable-theme 'solarized-dark)
                        (show-paren-mode -1)
                        (fringe-mode 0)))
        (:name deft
               :type git
               :url "git://jblevins.org/git/deft.git"
               :features deft
               :after (lambda ()
                        (setq deft-extension "org")
                        (setq deft-text-mode 'org-mode)
                        (global-set-key (kbd "<f8>") 'deft)
                        (let ((deft-dir (expand-file-name (concat user-home-directory "/.deft"))))
                          (if (not (file-exists-p deft-dir))
                              (make-symbolic-link org-directory
                                                  deft-dir)))))
        (:name ergoemacs-keybindings
               :before (lambda () (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us"))
               :after (lambda ()
                        (defun zane-ergoemacs-ido-minibuffer-setup-hook ()
                          (ergoemacs-ido-minibuffer-setup-hook)
                          ;; Swap the prev/next and prev/next-dir keys
                          ;; since we're displaying ido candidates
                          ;; vertically instead of horizontally
                          (define-key ergoemacs-ido-keymap ergoemacs-forward-char-key 'ido-next-match-dir) ; 'ido-prev-match-dir
                          (define-key ergoemacs-ido-keymap ergoemacs-backward-char-key 'ido-prev-match-dir) ; 'ido-prev-match
                          (define-key ergoemacs-ido-keymap ergoemacs-previous-line-key 'ido-prev-match) ; 'ido-next-match-dir
                          (define-key ergoemacs-ido-keymap ergoemacs-next-line-key 'ido-next-match) ; 'ido-prev-match-dir
                          )

                        (remove-hook 'ido-minibuffer-setup-hook 'ergoemacs-ido-minibuffer-setup-hook)
                        (add-hook 'ido-minibuffer-setup-hook 'zane-ergoemacs-ido-minibuffer-setup-hook)
                        (ergoemacs-mode 1)))
	(:name framemove
               :type emacswiki
               :features framemove
               :after (lambda ()
                        ;; try moving frames if moving windows fails
                        (setq framemove-hook-into-windmove t)))
        full-ack
	(:name magit
               :after (lambda ()
                        (global-set-key (kbd "C-c C-g") 'magit-status)))
	markdown-mode
        (:name org-mode
               :after (lambda ()
                        (setq org-default-notes-file (concat org-directory "/inbox.org"))
                        (define-key global-map (kbd "<f7>") 'org-capture)))
        (:name paredit
               :type http
               :url "http://mumble.net/~campbell/emacs/paredit.el"
               :features "paredit"
               :after (lambda ()
                        (defvar paredit-no-space-list '(python-mode)
                          "The list of major modes for which paredit should refrain appending a space
                            when inserting a matching delimiter.")

                        (add-to-list 'paredit-space-for-delimiter-predicates
                                     (lambda (endp delimiter)
                                       (not (member major-mode paredit-no-space-list))))

                        ;; C-j conflicts with windmove-left
                        (define-key paredit-mode-map (kbd "C-j") nil)
                        ;; C-k conflicts with windmove-down
                        (define-key paredit-mode-map (kbd "C-k") nil)
                        ;; C-; conflicts with searching
                        (define-key paredit-mode-map (kbd "M-;") nil)
                        ;; M-J conflicts with moving to the beginning
                        ;; of the file
                        (define-key paredit-mode-map (kbd "M-J") nil)
                        ))
	rinari
	slime
        (:name coffee-mode
               :type git
               :url "https://github.com/defunkt/coffee-mode.git"
               :features coffee-mode
               :post-init (lambda ()
                            (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
                            (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
                            ;; it defaults to js2-mode, which is not present in Emacs by default
                            (setq coffee-js-mode 'javascript-mode))
               :after (add-hook 'coffee-mode-hook
                                '(lambda () (set (make-local-variable 'tab-width) 2))))
        (:name clojure-mode
               :type elpa
               :after (lambda ()
                        (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)))
        (:name dired+
               :type emacswiki
               :after (lambda ()
                        (add-to-list 'font-lock-maximum-decoration '(dired-mode . nil))))
        (:name dired-isearch
               :type emacswiki
               :after (lambda ()
                        (require 'dired-isearch)
                        (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
                        (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
                        (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
                        (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)))
        ;; ensime ; fails with compilation error
        (:name flymake-cursor :type emacswiki :features flymake-cursor)
        (:name rainbow-delimiters
               :type emacswiki
               :features rainbow-delimiters
               :after (lambda ()
                        (defun zane-turn-on-rainbow-delimiters-mode ()
                          (interactive)
                          (rainbow-delimiters-mode 1))

                        (setq-default frame-background-mode 'dark)
                        (let ((supported-modes '(emacs-lisp-mode-hook
                                                 clojure-mode-hook
                                                 javascript-mode-hook
                                                 lisp-mode-hook
                                                 python-mode-hook)))
                          (dolist (hook supported-modes)
                            (add-hook hook 'zane-turn-on-rainbow-delimiters-mode)))))
	(:name speck
               :type emacswiki
               :features speck)
        (:name undo-tree
               :features undo-tree
               :before (lambda ()
                         (setq undo-tree-map (make-sparse-keymap))
                         ;; remap `undo' and `undo-only' to `undo-tree-undo'
                         (define-key undo-tree-map [remap undo] 'undo-tree-undo)
                         (define-key undo-tree-map [remap undo-only] 'undo-tree-undo)
                         ;; redo doesn't exist normally, so define our own keybindings
                         (define-key undo-tree-map (kbd "M-Z") 'undo-tree-redo)
                         ;; just in case something has defined `redo'...
                         (define-key undo-tree-map [remap redo] 'undo-tree-redo)
                         ;; we use "C-x u" for the undo-tree visualizer
                         (define-key undo-tree-map (kbd "\C-x u") 'undo-tree-visualize)
                         ;; bind register commands
                         (define-key undo-tree-map (kbd "C-x r u")
                           'undo-tree-save-state-to-register)
                         (define-key undo-tree-map (kbd "C-x r U")
                           'undo-tree-restore-state-from-register))
               :after (lambda () (global-undo-tree-mode)))
        (:name linum-off
               :type emacswiki
               :features linum-off
               :after (lambda ()
                        (dolist (mode '(fundamental-mode
                                        magit-mode))
                          (add-to-list 'linum-disabled-modes-list mode))))
        (:name scala-mode)
        (:name smex
               :after (lambda ()
                        (setq smex-save-file (concat user-emacs-directory ".smex-items"))
                        (setq smex-prompt-string "M-a ")
                        (global-set-key (kbd "M-a") 'smex)
                        (global-set-key (kbd "M-A") 'smex-major-mode-commands)
                        (global-set-key (kbd "C-c C-c M-a") 'execute-extended-command)))
	(:name starter-kit :type elpa
               :after (lambda ()
                        (add-hook 'text-mode-hook (lambda () (speck-mode t)))
                        (add-hook 'text-mode-hook (lambda () (visual-line-mode t)))
                        (remove-hook 'text-mode-hook 'turn-on-auto-fill)
                        (remove-hook 'text-mode-hook 'turn-on-flyspell)

                        (add-hook 'esk-coding-hook (lambda () (setq truncate-lines t)))
                        (remove-hook 'esk-coding-hook 'esk-turn-on-hl-line-mode)
                        (remove-hook 'emacs-lisp-mode-hook 'esk-turn-on-paredit)))
	(:name starter-kit-js   :type elpa)
	(:name starter-kit-lisp :type elpa)
	(:name starter-kit-ruby :type elpa)
        ))

(el-get)

(provide 'zane-el-get)
