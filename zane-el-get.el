;; Get el-get and install it if we don't have it already.
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(require 'el-get)

;; Add custom package.el sources (mainly for starter-kit)
(dolist (source '(
		  ("technomancy" . "http://repo.technomancy.us/emacs/")
                  ;; ("elpa" . "http://tromey.com/elpa/")
		  ))
  (add-to-list 'package-archives source t))
(package-initialize)

(setq el-get-sources
      '(
	color-theme
	color-theme-solarized
	color-theme-zenburn
	full-ack
	magit
	markdown-mode
	paredit
	rinari
	slime
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
        (:name rainbow-delimiters
               :type emacswiki
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
                            (add-hook hook 'zane-turn-on-rainbow-delimiters-mode)))
                        (require 'rainbow-delimiters)))
	(:name speck-mode :type emacswiki)
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
	))

(el-get 'wait)

(provide 'zane-el-get)
