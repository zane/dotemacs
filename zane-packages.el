;; Since this is used in multiple places:
(setq org-directory (expand-file-name (concat user-dropbox-directory
                                              "/Documents/deft")))

;; Add custom package.el sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("technomancy" . "http://repo.technomancy.us/emacs/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar z:my-packages
  '(
    ace-jump-mode
    auto-complete
    clojure-mode
    coffee-mode
    dired+
    ;;dired-isearch
    ergoemacs-keybindings
    flymake
    flymake-cursor
    full-ack
    git-blame
    helm
    helm-git
    ;;linum-off
    magit
    markdown-mode
    org
    paredit
    paredit
    rainbow-delimiters
    scala-mode
    slime
    ;; slime-repl
    smex
    solarized-theme
    ;;speck
    starter-kit
    starter-kit-js
    starter-kit-lisp
    starter-kit-ruby
    undo-tree
    yasnippet
    ))

;;; install missing packages
(let ((z:not-installed (remove-if 'package-installed-p z:my-packages)))
  (if z:not-installed
      (if (y-or-n-p "there are packages to be installed. install them? ")
          (dolist (package z:my-packages)
            (when (not (package-installed-p package))
              (package-install package))))))

;;; initialize packages
(setq z:package-init-dir (concat user-emacs-directory "zane-package-init/"))
(message (format "initializing packages out of %s" z:package-init-dir))
  (dolist (package z:my-packages)
    (let* ((initfile (concat z:package-init-dir (format "init-%s.el" package))))
      (message (format "initfile: %s" initfile))
      (if (and (package-installed-p package)
               (file-exists-p initfile))
          (progn (load initfile)
                 (message (format "zane-packages: loaded %s" initfile))))))

;;; FIXME: submit this to MELPA
(add-to-list 'load-path (concat user-emacs-directory "opt"))
(require 'framemove)
(load (concat z:package-init-dir "init-framemove.el"))

(provide 'zane-packages)
