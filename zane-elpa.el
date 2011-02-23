;;; zane-elpa.el --- Install a base set of packages automatically.
;; 
;; Based on starter-kit-elpa.el from the Emacs Starter Kit
;; https://github.com/technomancy/emacs-starter-kit

(require 'cl)

(defvar zane-packages '(clojure-test-mode
                        color-theme
			css-mode
			find-file-in-project
			gist
			idle-highlight
			magit
			paredit
			project-local-variables
			slime
			slime-repl
                        clojure-mode
                        starter-kit
                        starter-kit-lisp)
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

(defun esk-online? ()
  "See if we're online.

  Windows does not have the network-interface-list function, so we just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; On your first run, this should pull in all the base packages.
(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents))
  (zane-elpa-install))

(provide 'zane-elpa)
