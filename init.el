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

(add-to-list 'load-path (concat dotfiles-dir "/lib/package.github.technomancy"))
(require 'package)
(setq package-user-dir (concat dotfiles-dir "/elpa"))

(require 'package)
(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(require 'zane-elpa)

;; Emacs Starter Kit

(add-hook 'text-mode-hook (lambda () (speck-mode t)))
(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-flyspell)

(add-hook 'esk-coding-hook (lambda () (setq truncate-lines t)))
(remove-hook 'esk-coding-hook 'esk-turn-on-hl-line-mode)

(setq-default truncate-lines t)

(require 'speck)

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

;; Auto-save

(setq temporary-file-directory (concat dotfiles-dir "/backup"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
      
;; Everything else:

(setq server-use-tcp t)

;; dired
(require 'image-dired)

;; Clojure
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup) ; what it sounds like

;; Markdown
(require 'markdown-mode)

;; Set font
;; TODO: Make this conditional based on os
(set-default-font "-apple-inconsolata-medium-r-normal--13-130-72-72-m-130-iso10646-1")
;(set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")

(require 'color-theme)
(load-file (concat dotfiles-dir "lib/color-theme-vibrant-ink.github.mig/color-theme-vibrant-ink.el" ))
(color-theme-vibrant-ink)

(setq sentence-end-double-space nil)

(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

(require 'yaml-mode)
