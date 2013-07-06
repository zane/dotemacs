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
    ace-jump-mode
    ace-jump-buffer
    ack-and-a-half
    auto-complete
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
    iedit
    ido-ubiquitous
    ido-vertical-mode
    jedi
    key-chord
    kibit-mode
    magit
    multiple-cursors
    nrepl
    org
    paredit
    rainbow-delimiters
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
    yasnippet
    ))

;; https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el
(defvar zane-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("Gemfile" ruby-mode ruby-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.jade\\'" jade-mode jade-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.php\\'" php-mode php-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rake\\'" ruby-mode ruby-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode))
  "(extension, package, mode) tuples")

(defmacro zane-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (zane-auto-install extension package mode))))
 zane-auto-install-alist)

(defun z:install-missing-packages ()
  (interactive)
  (let ((not-installed (remove-if 'package-installed-p z:my-packages)))
    (when (and not-installed
               (y-or-n-p (format "there are %d packages to be installed. install them? " (length not-installed))))
      (package-refresh-contents)
      (dolist (package z:my-packages)
        (when (not (package-installed-p package))
          (package-install package))))))

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
