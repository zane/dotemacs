;; Inspirational initfiles:
;;     https://raw.github.com/gist/304391/0f5dd9acb959bcb3a244c2ad903bec75096cab17/.emacs.el

;; TODO: http://www.dr-qubit.org/undo-tree/undo-tree.el
;; TODO: http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; TODO: http://jesselegg.com/archives/2010/03/14/emacs-python-programmers-2-virtualenv-ipython-daemon-mode/

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))


(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(fringe-mode 0)
(delete-selection-mode 1)
(setq redisplay-dont-pause t)
(setq server-use-tcp t)
(setq font-lock-maximum-decoration (list (cons t t)))
(setq ns-pop-up-frames nil)
(setq initial-scratch-message "")
(display-battery-mode 1)
(setq ido-create-new-buffer 'always)
(setq-default fill-column 80)

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
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(when window-system
  (let ((default-font (if (z:mac-p) "Source Code Pro 12" "Monospace 10"))) ;; "Inconsolata-13"
    (set-face-font 'default default-font))
  (show-paren-mode 1)
  (fringe-mode 0))

(after 'paren
  (defadvice load-theme (after load-theme-advice activate)
    (set-face-background 'show-paren-match nil)
    (set-face-foreground 'show-paren-match nil)
    (set-face-inverse-video-p 'show-paren-match nil)
    (set-face-attribute 'show-paren-match nil :underline (face-background 'cursor))))

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
(z:install-missing-packages)
(z:initialize-packages)

(require 'zane-keybindings)
(add-to-list 'default-frame-alist '(cursor-color . "#d33682"))

;; http://www.masteringemacs.org/articles/2010/10/18/maximizing-emacs-startup/
(if (and (z:mac-p)
         window-system)
    (add-hook 'window-setup-hook 'ns-toggle-fullscreen t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes (quote ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#282a2e")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#cc6666") (40 . "#de935f") (60 . "#f0c674") (80 . "#b5bd68") (100 . "#8abeb7") (120 . "#81a2be") (140 . "#b294bb") (160 . "#cc6666") (180 . "#de935f") (200 . "#f0c674") (220 . "#b5bd68") (240 . "#8abeb7") (260 . "#81a2be") (280 . "#b294bb") (300 . "#cc6666") (320 . "#de935f") (340 . "#f0c674") (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))
(put 'dired-find-alternate-file 'disabled nil)
