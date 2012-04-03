(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

;; Get el-get and install it if we don't have it already.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get") (unless (require 'el-get nil t) (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el" (lambda (s) (end-of-buffer) (eval-print-last-sexp))))

(require 'el-get)

;; Since this is used in multiple places:
(setq org-directory (expand-file-name (concat user-dropbox-directory
                                              "/Documents/deft")))

;; Add custom package.el sources (mainly for starter-kit)
(add-to-list 'package-archives '("technomancy" . "http://repo.technomancy.us/emacs/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
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
        (:name ergoemacs-keybindings
               :before (lambda () (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us"))
               :after (lambda ()
                        (defun zane-ergoemacs-ido-minibuffer-setup-hook ()
                          (ergoemacs-ido-minibuffer-setup-hook)
                          ;; Swap the prev/next and prev/next-dir keys
                          ;; since we're displaying ido candidates
                          ;; vertically instead of horizontally
                          (define-key ergoemacs-ido-keymap ergoemacs-forward-char-key 'ido-next-match-dir) ; was 'ido-prev-match-dir
                          (define-key ergoemacs-ido-keymap ergoemacs-backward-char-key 'ido-prev-match-dir) ; was 'ido-prev-match
                          (define-key ergoemacs-ido-keymap ergoemacs-previous-line-key 'ido-prev-match) ; was 'ido-next-match-dir
                          (define-key ergoemacs-ido-keymap ergoemacs-next-line-key 'ido-next-match) ; was 'ido-prev-match-dir
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
	(:name markdown-mode
               :after (lambda ()
                        (add-to-list 'auto-mode-alist '(".mkd" . markdown-mode))))
        (:name org-mode
               :after (lambda ()
                        (setq org-default-notes-file (concat org-directory "/inbox.org"))
                        (define-key global-map (kbd "<f7>") 'org-capture)))
        (:name paredit
               :type http
               :url "http://mumble.net/~campbell/emacs/paredit.el"
               :features "paredit"
               :after (lambda ()
                        (defvar paredit-no-space-list '(python-mode
                                                        html-mode)
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

                        (define-key paredit-mode-map (kbd "C-d") nil) ; was paredit-forward-delete
                        (define-key paredit-mode-map (kbd "M-f") 'paredit-forward-delete)

                        (define-key paredit-mode-map (kbd "C-d") nil) ; was paredit-backward-delete
                        (define-key paredit-mode-map (kbd "M-d") 'paredit-backward-delete)

                        (define-key paredit-mode-map (kbd "M-d") nil) ; was paredit-backward-kill-word
                        (define-key paredit-mode-map (kbd "M-e") 'paredit-backward-kill-word)

                        (define-key paredit-mode-map (kbd "M-d") nil) ; was paredit-forward-kill-word
                        (define-key paredit-mode-map (kbd "M-r") 'paredit-forward-kill-word)

                        (define-key paredit-mode-map (kbd "C-k") nil) ; was paredit-kill
                        (define-key paredit-mode-map (kbd "M-g") 'paredit-kill)))
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
        (:name solarized-theme
               :type elpa
               :after (lambda ()
                        ;(enable-theme 'solarized-dark)
                        (show-paren-mode -1)
                        (fringe-mode 0)))
        (:name clojure-mode
               :type elpa
               :after (lambda ()
                        (add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)))
        (:name dired+
               :type elpa
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
        (:name flymake
               :type elpa
               :after (lambda ()
                        (defun flymake-xml-init ()
                          (list "xmllint"
                                (list "--valid"
                                      (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))
                        (defun flymake-html-init ()
                          (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                             'flymake-create-temp-inplace))
                                 (local-file (file-relative-name
                                              temp-file
                                              (file-name-directory buffer-file-name))))
                            (list "tidy" (list local-file))))

                        (add-to-list 'flymake-allowed-file-name-masks
                                     '("\\.html$\\|\\.ctp" flymake-html-init))

                        (add-to-list 'flymake-err-line-patterns
                                     '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
                                       nil 1 2 4))))
        (:name flymake-cursor :type elpa)
        (:name gist :type elpa)
        (:name rainbow-delimiters
               :type elpa
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
	(:name speck :type elpa)
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
                        (add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
                        (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
                        (remove-hook 'emacs-lisp-mode-hook 'esk-turn-on-paredit)))
	(:name starter-kit-js   :type elpa)
	(:name starter-kit-lisp :type elpa)
	(:name starter-kit-ruby :type elpa)))

(el-get 'sync)

(provide 'zane-el-get)
