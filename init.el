;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load path etc.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; Load up ELPA, the package manager

(setq package-user-dir (concat dotfiles-dir "elpa"))

(eval-after-load 'package
  '(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
			    ("elpa" . "http://tromey.com/elpa/")))
	    (add-to-list 'package-archives source t)))
(package-initialize)
(require 'zane-elpa)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; Autoload any customizations in the customizations directory 

(mapc '(lambda (file)
	 (eval-after-load )
      (directory-files (concat dotfiles-dir "conf") nil "^.*el$")
      
;; Everything else

(enable-theme 'tango-dark)

(setq mac-command-modifier 'meta)

(global-set-key (kbd "C-x y") 'bury-buffer)
(global-set-key "\C-o" 'other-window)

(global-set-key "\M-r" 'isearch-backward)
(global-set-key "\M-s" 'isearch-forward)

;; Key for fullscreen from custom build of Emacs.app:
;; http://www.stratospark.com/blog/2010/fullscreen_emacs_on_osx.html
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)

(setq server-use-tcp t)

(iswitchb-mode)

(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

