;; Since this is used in multiple places:
(setq org-directory (expand-file-name (concat user-dropbox-directory
                                              "/Documents/deft")))

;; Add custom package.el sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar z:my-packages
  '(
    ;;http://www.nongnu.org/geiser/
    ace-jump-mode
    ace-jump-buffer
    ack-and-a-half
    auto-complete
    clojure-mode
    coffee-mode
    diminish
    dired+
    ediff
    edit-server
    ergoemacs-keybindings
    exec-path-from-shell
    expand-region
    find-dired
    flycheck
    framemove
    fuzzy
    gist
    git-blame
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    gnus
    ido-vertical-mode
    jedi
    json-mode
    key-chord
    kibit-mode
    magit
    markdown-mode
    multiple-cursors
    nrepl
    org
    paredit
    rainbow-delimiters
    ruby-mode
    scala-mode
    slime
    slime-repl
    smartparens
    smex
    solarized-theme
    starter-kit
    starter-kit-lisp
    tramp
    undo-tree
    volatile-highlights
    whitespace
    windmove
    yaml-mode
    yasnippet
    ))

(defun z:install-missing-packages ()
  (interactive)
  (let ((not-installed (remove-if 'package-installed-p z:my-packages)))
    (if not-installed
        (if (y-or-n-p (format "there are %d packages to be installed. install them? " (length not-installed)))
            (progn (package-refresh-contents)
                   (dolist (package z:my-packages)
                     (when (not (package-installed-p package))
                       (package-install package))))))))

(defun z:initialize-packages ()
  (interactive)
  (setq z:package-init-dir (concat user-emacs-directory "zane-package-init/"))
  (message (format "initializing packages out of %s" z:package-init-dir))
  (dolist (package (append (mapcar 'car package--builtins) package-activated-list))
    (let* ((initfile (concat z:package-init-dir (format "init-%s.el" package))))
      (if (and (package-installed-p package)
               (file-exists-p initfile))
          (progn (load initfile)
                 (message (format "loaded %s" initfile)))))))

(when (z:mac-p)
  (exec-path-from-shell-initialize))

(provide 'zane-packages)
