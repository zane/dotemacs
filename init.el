;; Inspirational initfiles:
;;     https://raw.github.com/gist/304391/0f5dd9acb959bcb3a244c2ad903bec75096cab17/.emacs.el

;; TODO: http://www.dr-qubit.org/undo-tree/undo-tree.el
;; TODO: http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; TODO: http://jesselegg.com/archives/2010/03/14/emacs-python-programmers-2-virtualenv-ipython-daemon-mode/

(require 'cl)

(defvar user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "The user's home directory.")

(defvar zane-projects-dir
  (expand-file-name (concat user-home-directory "Projects/"))
  "The directory containing the user's checked out source code.")

(defvar user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "The user's Dropbox root directory.")

(add-to-list 'load-path user-emacs-directory)

;; http://www.gnu.org/software/emacs/elisp/html_node/Simple-Match-Data.html#Simple-Match-Data

(require 'zane-funcs)

;; auto-save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(setq temporary-file-directory (concat user-emacs-directory "tmp"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EverythingElse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default truncate-lines t)
(setq sentence-end-double-space nil)
(global-auto-revert-mode t)
(global-subword-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode 0)
(delete-selection-mode 1)
(setq redisplay-dont-pause t)
(setq server-use-tcp t)
(setq font-lock-maximum-decoration (list (cons t t)))
(setq ns-pop-up-frames nil)
(setq initial-scratch-message "")
(display-battery-mode 1)
(setq ido-create-new-buffer 'always)

;; Enable disabled commands
(setq enable-recursive-minibuffers t)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Tooltips
;; (http://www.masteringemacs.org/articles/2010/10/15/making-tooltips-appear-in-echo-area/)
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; Prompts
;; (http://www.masteringemacs.org/articles/2010/11/14/disabling-prompts-emacs/)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(if window-system
    (if (z:mac-p)
        (set-face-font 'default "Inconsolata-13")
      (set-face-font 'default "Monospace-10")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load files in config/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(require 'zane-keybindings)

;; http://www.masteringemacs.org/articles/2010/10/18/maximizing-emacs-startup/
(if (and (z:mac-p)
         window-system)
    (add-hook 'window-setup-hook 'ns-toggle-fullscreen t))
