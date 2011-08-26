(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

;; Get el-get and install it if we don't have it already.
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(require 'el-get)

;; Add custom package.el sources (mainly for starter-kit)
(add-to-list 'package-archives '("technomancy" . "http://repo.technomancy.us/emacs/") t)
(package-initialize)

(setq el-get-sources
      '((:name color-theme-solarized
               :features solarized-dark-theme
               :after (lambda ()
                        (enable-theme 'solarized-dark)
                        (show-paren-mode -1)
                        (fringe-mode 0)))
	full-ack
	(:name magit
               :after (lambda ()
                        (global-set-key (kbd "C-c C-g") 'magit-status)))
	markdown-mode
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
                        (define-key paredit-mode-map (kbd "M-J") 'paredit-backward)
                        (define-key paredit-mode-map (kbd "M-L") 'paredit-forward)
                        (define-key paredit-mode-map (kbd "M-I") 'paredit-backward-up)
                        (define-key paredit-mode-map (kbd "M-K") 'paredit-forward-down)

                        (define-key paredit-mode-map (kbd "C-j") nil)))
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
        (:name linum-off
               :type emacswiki
               :features linum-off
               :after (lambda ()
                        (dolist (mode '(fundamental-mode
                                        magit-mode))
                          (add-to-list 'linum-disabled-modes-list mode))))
	(:name starter-kit :type elpa
               :after (lambda ()
                        (add-hook 'text-mode-hook (lambda () (speck-mode t)))
                        (add-hook 'text-mode-hook (lambda () (visual-line-mode t)))
                        (remove-hook 'text-mode-hook 'turn-on-auto-fill)
                        (remove-hook 'text-mode-hook 'turn-on-flyspell)

                        (add-hook 'esk-coding-hook (lambda () (setq truncate-lines t)))
                        (remove-hook 'esk-coding-hook 'esk-turn-on-hl-line-mode)))
	(:name starter-kit-js   :type elpa)
	(:name starter-kit-lisp :type elpa)
	(:name starter-kit-ruby :type elpa)
        (:name framemove
               :type emacswiki
               :features framemove
               :after (lambda ()
                        (setq framemove-hook-into-windmove t)))))

(el-get)

(provide 'zane-el-get)
