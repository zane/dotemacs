(setq mac-command-modifier 'meta)

(global-set-key (kbd "C-x y") 'bury-buffer)
(global-set-key "\C-o" 'other-window)

;; Key for fullscreen from custom build of Emacs.app:
;; http://www.stratospark.com/blog/2010/fullscreen_emacs_on_osx.html
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; Load up ELPA, the package manager
;; Tricks below here stolen from the Emacs Starter Kit:
;; https://github.com/technomancy/emacs-starter-kit

(add-to-list 'load-path package-github-technomancy-path)
(setq package-user-dir (concat dotfiles-dir "/elpa"))

(require 'package)
(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(require 'zane-elpa)

;; Emacs Starter Kit

(remove-hook 'esk-coding-hook 'esk-turn-on-hl-line-mode)

;; Autoload any configuration files in the conf directory.

;; (mapc (function 
;;        (lambda (file)
;; 	 (let ((name
;; 		(file-name-sans-extension
;; 		 (file-name-nondirectory file))))
;; 	   (eval-after-load name
;; 	     `(load ,(concat dotfiles-dir (concat "conf/" file)))))))
;;       (directory-files (concat dotfiles-dir "conf") nil "^.*el$"))
;;;; (string-match "\\(.?\\)ar" "foobar")
;;;; (match-string 1 "foobar")
;; http://www.gnu.org/software/emacs/elisp/html_node/Simple-Match-Data.html#Simple-Match-Data
      
;; Everything else:

(enable-theme 'tango-dark)

(setq server-use-tcp t)

;; Clojure
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup) ; what it sounds like
;; TODO: Find out where swank-clojure can be installed globally
(setq swank-clojure-classpath (list "/usr/local/Cellar/clojure/1.2.0/clojure.jar" "/usr/local/Cellar/clojure-contrib/1.2.0/clojure-contrib.jar" "swank-clojure.jar"))

;; Set font
;; TODO: Make this conditional based on os
(set-default-font "-apple-inconsolata-medium-r-normal--13-130-72-72-m-130-iso10646-1")
;(set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
