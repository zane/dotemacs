;; TODO: http://www.nongnu.org/geiser/
;; TODO: Look into this https://raw.github.com/gist/304391/0f5dd9acb959bcb3a244c2ad903bec75096cab17/.emacs.el
;; TODO: http://tapoueh.org/blog/2011/07/29-emacs-ansi-colors.html
;; TODO: http://xahlee.blogspot.com/2009/08/how-to-use-and-setup-emacss-whitespace.html
;; TODO: http://www.dr-qubit.org/undo-tree/undo-tree.el
;; TODO: http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; TODO: http://jesselegg.com/archives/2010/03/14/emacs-python-programmers-2-virtualenv-ipython-daemon-mode/

(require 'cl)

(setq-default truncate-lines t)
(setq sentence-end-double-space nil)
(global-auto-revert-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)

(defvar user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "The user's home directory.")

(defvar user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "The user's Dropbox root directory.")

(add-to-list 'load-path user-emacs-directory)

;; http://www.gnu.org/software/emacs/elisp/html_node/Simple-Match-Data.html#Simple-Match-Data

;; auto-save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(setq temporary-file-directory (concat user-emacs-directory "tmp"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Everything else:
(setq redisplay-dont-pause t)
(setq server-use-tcp t)
(delete-selection-mode 1)
(setq font-lock-maximum-decoration (list (cons t t)))
(setq ns-pop-up-frames nil)
(setq initial-scratch-message "")

(if (string-match "apple-darwin" system-configuration)
    (set-face-font 'default "Inconsolata-13")
  (set-face-font 'default "Monospace-10"))

;; Load all the files in the config dir
(progn
  (setq zane-emacs-config-dir (concat user-emacs-directory "config/"))
  (when (file-exists-p zane-emacs-config-dir)
    (dolist (l (directory-files zane-emacs-config-dir nil "^[^#].*el$"))
      (load (concat zane-emacs-config-dir l)))))

;; On Mac OS X, Emacs sessions launched from the GUI don't always
;; respect your configured $PATH. If Emacs can't find lein, you may
;; need to give it some help. The quickest way is probably to add this
;; elisp to your config:
(setenv "PATH" (shell-command-to-string "echo $PATH"))

(require 'zane-packages)
(require 'zane-funcs)
(require 'zane-keybindings)
