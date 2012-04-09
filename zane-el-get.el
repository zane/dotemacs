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

(setq el-get-user-package-directory (expand-file-name "zane-el-get-package-init" user-emacs-directory))

(setq el-get-sources
      '(ergoemacs-keybindings
	(:name framemove :type emacswiki :features framemove)
        full-ack
	magit
	markdown-mode
        (:name auto-complete :type elpa :depends fuzzy)
        auto-complete-yasnippet
        (:name fuzzy :type elpa)
        (:name org :type elpa)
        (:name paredit
               :type http
               :url "http://mumble.net/~campbell/emacs/paredit.el"
               :features "paredit")
	slime
        (:name coffee-mode :type elpa)
        (:name solarized-theme :type elpa)
        (:name clojure-mode :type elpa)
	dired+
        (:name dired-isearch
               :type emacswiki
               :after (lambda ()
                        (require 'dired-isearch)
                        (define-key dired-mode-map (kbd "C-s") 'dired-isearch-forward)
                        (define-key dired-mode-map (kbd "C-r") 'dired-isearch-backward)
                        (define-key dired-mode-map (kbd "ESC C-s") 'dired-isearch-forward-regexp)
                        (define-key dired-mode-map (kbd "ESC C-r") 'dired-isearch-backward-regexp)))
        (:name flymake :type elpa)
        (:name flymake-cursor :type elpa)
        (:name rainbow-delimiters :type elpa)
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
        (:name linum-off :type elpa)
        scala-mode
        smex
	(:name starter-kit :type elpa :depends paredit)
	(:name starter-kit-js :type elpa)
	(:name starter-kit-lisp :type elpa)
	(:name starter-kit-ruby :type elpa)
        (:name yasnippet :type elpa)))

(setq el-get-verbose 1)
(el-get 'sync)

(provide 'zane-el-get)
