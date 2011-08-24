;;; zane-elpa.el --- Install a base set of packages automatically.
;;
;; Based on starter-kit-elpa.el from the Emacs Starter Kit
;; https://github.com/technomancy/emacs-starter-kit

;; Load up ELPA, the package manager
;; Tricks below here stolen from the Emacs Starter Kit:
;; https://github.com/technomancy/emacs-starter-kit

(add-to-list 'load-path (concat user-emacs-directory "lib/package.github.technomancy"))
(require 'package)
(setq zane-elpa-dir (concat user-emacs-directory "elpa"))

(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(defvar zane-packages '(
			find-file-in-project
			gist
			idle-highlight
			magit
			paredit
			project-local-variables
			slime
			slime-repl
                        clojure-mode
                        clojure-test-mode
                        color-theme
                        css-mode
                        dired-isearch
                        full-ack
                        inf-ruby
                        markdown-mode
                        rinari
                        starter-kit
                        starter-kit-js
                        starter-kit-lisp
                        starter-kit-ruby
                        yaml-mode
                        )
  "Libraries that should be installed by default.")

(defun zane-elpa-install ()
  "Install all zane packages that aren't installed."
  (interactive)
  (unless (file-exists-p package-user-dir) (package-refresh-contents))
  (dolist (package zane-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun zane-online? ()
  "See if we're online.

  Windows does not have the network-interface-list function, so
  we just have to assume it's online."
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; On your first run, this should pull in all the base packages.
(when (zane-online?)
  (unless package-archive-contents (package-refresh-contents))
  (zane-elpa-install))

(provide 'zane-elpa)
