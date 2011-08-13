;; TODO: http://www.nongnu.org/geiser/
;; TODO: http://emacs-fu.blogspot.com/2011/05/toward-balanced-and-colorful-delimiters.html
;; TODO: https://github.com/djcb/elisp/blob/master/themes/zenburn-theme.el

(setq mac-command-modifier 'meta)
(global-set-key (kbd "C-x y") 'bury-buffer)
(global-set-key (kbd "C-o")  'other-window)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen) ; http://www.stratospark.com/blog/2010/fullscreen_emacs_on_osx.html
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)

(setq-default truncate-lines t)
(setq sentence-end-double-space nil)
(global-auto-revert-mode t)

(setq zane-emacs-root (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path zane-emacs-root)

;; Load up ELPA, the package manager
;; Tricks below here stolen from the Emacs Starter Kit:
;; https://github.com/technomancy/emacs-starter-kit

(add-to-list 'load-path (concat zane-emacs-root "/lib/package.github.technomancy"))
(require 'package)
(setq zane-elpa-dir (concat zane-emacs-root "/elpa"))

(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(require 'zane-elpa)

;; http://www.gnu.org/software/emacs/elisp/html_node/Simple-Match-Data.html#Simple-Match-Data

;; Auto-save

(setq temporary-file-directory (concat zane-emacs-root "/backup"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
      
;; Everything else:
(setq server-use-tcp t)
(delete-selection-mode 1)

;; Set font
;; TODO: Make this conditional based on os
(set-default-font "-apple-inconsolata-medium-r-normal--13-130-72-72-m-130-iso10646-1")
;(set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")

(require 'color-theme)
(load-file (concat zane-emacs-root "lib/color-theme-vibrant-ink.github.mig/color-theme-vibrant-ink.el"))
(color-theme-vibrant-ink)

(require 'speck)
(require 'markdown-mode)
(require 'rinari)
(require 'yaml-mode)

(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

(progn
  (setq zane-emacs-config-dir (concat zane-emacs-root "config/"))
  
  (when (file-exists-p zane-emacs-config-dir)
    (dolist (l (directory-files zane-emacs-config-dir nil "^[^#].*el$"))
      (load (concat zane-emacs-config-dir l)))))
