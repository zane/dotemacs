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
    ;;dired-isearch
    ;;linum-off
    ;;slime-repl
    ;;speck
    ;;yasnippet
    ;;http://www.nongnu.org/geiser/
    ace-jump-mode
    auto-complete
    clojure-mode
    coffee-mode
    dired+
    ediff
    edit-server
    ergoemacs-keybindings
    find-dired
    flymake
    flymake-cursor
    framemove
    full-ack
    fuzzy
    gist
    git-blame
    gnus
    helm
    helm-git
    magit
    markdown-mode
    org
    paredit
    rainbow-delimiters
    scala-mode
    slime
    slime-repl
    smex
    solarized-theme
    speck
    starter-kit
    starter-kit-js
    starter-kit-lisp
    starter-kit-ruby
    tramp
    undo-tree
    whitespace
    windmove
    ))

;;; install missing packages
(let ((not-installed (remove-if 'package-installed-p z:my-packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? " (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package z:my-packages)
                   (when (not (package-installed-p package))
                     (package-install package)))))))

;;; initialize packages
(setq z:package-init-dir (concat user-emacs-directory "zane-package-init/"))
(message (format "initializing packages out of %s" z:package-init-dir))
  (dolist (package z:my-packages)
    (let* ((initfile (concat z:package-init-dir (format "init-%s.el" package))))
      (if (and (package-installed-p package)
               (file-exists-p initfile))
          (progn (load initfile)
                 (message (format "loaded %s" initfile))))))

(provide 'zane-packages)
