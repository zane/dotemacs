;; TODO: http://www.nongnu.org/geiser/
;; TODO: Look into this https://raw.github.com/gist/304391/0f5dd9acb959bcb3a244c2ad903bec75096cab17/.emacs.el
(require 'cl)

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

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(setq temporary-file-directory (concat zane-emacs-root "/backup"))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Everything else:
(setq server-use-tcp t)
(delete-selection-mode 1)
(setq font-lock-maximum-decoration (list (cons t t)))

;; Set font
;; TODO: Make this conditional based on os
;;(set-face-attribute 'default nil :font "Consolas-14")
(set-face-attribute 'default nil :font "Inconsolata-13")

(require 'speck)
(require 'markdown-mode)
(require 'rinari)
(require 'yaml-mode)

(require 'zane-funcs)
(require 'zane-keys)

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(progn
  (setq zane-emacs-config-dir (concat zane-emacs-root "config/"))

  (when (file-exists-p zane-emacs-config-dir)
    (dolist (l (directory-files zane-emacs-config-dir nil "^[^#].*el$"))
      (load (concat zane-emacs-config-dir l)))))
