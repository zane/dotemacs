;;; personal --- Summary
;;;
;;; Commentary:

;; https://github.com/technomancy/nrepl-discover
;; https://github.com/magnars/js2-refactor.el
;; TODO: http://www.wisdomandwonder.com/article/8105/a-reminder-for-a-library-config
;; TODO: https://github.com/bartman/git-wip

;;; Code:
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(prelude-ensure-module-deps
 '(ac-nrepl
   ack-and-a-half
   better-defaults
   clojure-snippets
   diminish
   evil
   f
   framemove
   git-timemachine
   haskell-mode
   htmlize
   idle-highlight-mode
   iedit
   ido-vertical-mode
   json-mode
   jsx-mode
   golden-ratio
   midje-mode
   multiple-cursors
   pos-tip
   rainbow-delimiters
   rainbow-identifiers
   refheap
   slamhound
   smart-mode-line
   sql-indent
   use-package
   vlf
   visual-regexp-steroids
   windmove
   yasnippet))

(setq prelude-flyspell nil)
(setq prelude-guru nil)

(require 'f)
(require 'use-package)

(defvar user-home-directory
  (f-expand ".." user-emacs-directory)
  "The user's home directory.")

(defvar user-projects-directory
  (f-join user-home-directory "Projects")
  "The user's projects directory.")

(defvar user-dropbox-directory
  (f-join "Dropbox" user-home-directory)
  "The user's Dropbox directory.")

(defun increment-number-at-point ()
  "Increments the number at point."
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Previously Migrated
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun z:mac-p ()
  "Truthy if the host OS is a Mac."
  (string-match "apple-darwin" system-configuration))

(when (z:mac-p)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(use-package solarized-theme
  :init (when window-system (load-theme 'solarized-dark t))
  :config (progn (setq solarized-use-variable-pitch nil)
                 (setq solarized-height-plus-1 1)
                 (setq solarized-height-plus-2 1)
                 (setq solarized-height-plus-3 1)
                 (setq solarized-height-plus-4 1)
                 (setq solarized-height-minus-1 1)
                 (setq solarized-distinct-fringe-background t)
                 (setq solarized-high-contrast-mode-line nil)))

(defcustom echo-area-bell-string "*DING* " ;"♪"
  "Message displayed in mode-line by `echo-area-bell' function."
  :group 'user)
(defcustom echo-area-bell-delay 0.1
  "Number of seconds `echo-area-bell' displays its message."
  :group 'user)
;; internal variables
(defvar echo-area-bell-cached-string nil)
(defvar echo-area-bell-propertized-string nil)
(defun echo-area-bell ()
  "Briefly display a highlighted message in the echo-area.
    The string displayed is the value of `echo-area-bell-string',
    with a red background; the background highlighting extends to the
    right margin.  The string is displayed for `echo-area-bell-delay'
    seconds.
    This function is intended to be used as a value of `ring-bell-function'."
  (unless (equal echo-area-bell-string echo-area-bell-cached-string)
    (setq echo-area-bell-propertized-string
          (propertize
           (concat
            (propertize
             "x"
             'display
             `(space :align-to (- right ,(+ 2 (length echo-area-bell-string)))))
            echo-area-bell-string)
           'face '(:background "white")))
    (setq echo-area-bell-cached-string echo-area-bell-string))
  (message echo-area-bell-propertized-string)
  (sit-for echo-area-bell-delay)
  (message ""))
(setq ring-bell-function 'echo-area-bell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions and Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun z:mac-p ()
  "Truthy if the host OS is a Mac."
  (string-match "apple-darwin" system-configuration))

(defun key-binding-at-point (key)
  (mapcar (lambda (keymap) (lookup-key keymap key))
          (cl-remove-if-not
           #'keymapp
           (append
            (mapcar (lambda (overlay)
                      (overlay-get overlay 'keymap))
                    (overlays-at (point)))
            (get-text-property (point) 'keymap)
            (get-text-property (point) 'local-map)))))

(defun locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEXT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun z/visual-line-mode-on ()
  "Turn on visual line mode."
  (visual-line-mode 1))

(remove-hook 'text-mode-hook 'abbrev-mode)
(dolist (hook '(z/visual-line-mode-on
                flyspell-mode))
  (add-hook 'text-mode-hook hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-scratch-message "")
(setq-default truncate-lines t)
(tooltip-mode -1)

(use-package smart-mode-line
  :if window-system
  :init (require 'smart-mode-line)
  
  :config
  (progn
    (add-to-list 'sml/replacer-regexp-list '("^~/Projects/" ":P:"))
    (sml/setup)))

(when window-system
  (let ((default-font (if (z:mac-p)
                          "-*-Anonymous Pro Minus-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"
                          ;; "DejaVu Sans Mono 11"
                          ;; "Source Code Pro 12"
                          ;; "Inconsolata-13"
                          "Monospace 10")))
    (set-face-font 'default default-font)
    (if (z:mac-p)
        (set-face-font 'variable-pitch "Avenir 13")))
  (scroll-bar-mode -1))

;; http://xahlee.blogspot.com/2009/08/how-to-use-and-setup-emacss-whitespace.html
(when window-system
  (setq whitespace-trailing-regexp
        "^.*[^\r\n\t \xA0\x8A0\x920\xE20\xF20]+\\([\t \xA0\x8A0\x920\xE20\xF20]+\\)$")
  (setq whitespace-style '(face tabs trailing empty))
  (setq whitespace-action '(auto-cleanup warn-if-read-only)))

(when window-system
  (after "rainbow-delimiters-autoloads"
    (setq-default frame-background-mode 'dark)
    (let ((hooks '(emacs-lisp-mode-hook
                   clojure-mode-hook
                   js-mode-hook
                   lisp-mode-hook
                   python-mode-hook)))
      (dolist (hook hooks)
        (add-hook hook 'rainbow-delimiters-mode-enable)))

    (after 'nrepl
      (add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode-enable)))
  (after "rainbow-identifiers-autoloads"
    (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)))

(after 'paren
  (set-face-background 'show-paren-match nil)
  (set-face-foreground 'show-paren-match nil)
  (set-face-inverse-video-p 'show-paren-match nil))

(after "diminish-autoloads"
  (defmacro rename-modeline (package-name mode new-name)
    `(eval-after-load ,package-name
       '(defadvice ,mode (after rename-modeline activate)
          (setq mode-name ,new-name))))

  ;; http://whattheemacsd.com//appearance.el-01.html
  (rename-modeline 'lisp-mode emacs-lisp-mode "EL")
  (rename-modeline 'lisp-mode lisp-interaction-mode "LI")
  (rename-modeline 'js js-mode "JS")

  (after 'flycheck
    (setq flycheck-mode-line-lighter " fl")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun turn-on-electric-indent-mode ()
  "Enables electric indent mode."
  (electric-indent-mode +1))

(add-hook 'emacs-lisp-mode-hook 'turn-on-electric-indent-mode)
(add-hook 'prog-mode-hook 'turn-on-electric-indent-mode)

(use-package flycheck
  :config (progn (set-face-attribute 'flycheck-error nil :underline "red")
                 (set-face-attribute 'flycheck-warning nil :underline "yellow")))

(use-package smartparens
  :config
  (progn
    (setq sp-base-key-bindings 'paredit)
    (sp-use-paredit-bindings)
    (setq sp-autoinsert-if-followed-by-word t)
    (setq sp-navigate-close-if-unbalanced t)
    
    ;; https://github.com/Fuco1/smartparens/wiki/Paredit-and-smartparens#random-differences
    (bind-key ")"   'sp-up-sexp            smartparens-mode-map)
    (bind-key "M-F" 'sp-kill-sexp          smartparens-mode-map)
    (bind-key "M-D" 'sp-backward-kill-sexp smartparens-mode-map)))

(use-package paxedit
  :diminish "pax"
  :commands paxedit-mode
  :config (progn (bind-key "C-M-t"         'paxedit-transpose-forward paxedit-mode-map)
                 (bind-key "M-<up>"        'paxedit-backward-up       paxedit-mode-map)
                 (bind-key "M-<down>"      'paxedit-backward-end      paxedit-mode-map)
                 (bind-key "M-o"           'paxedit-next-symbol       paxedit-mode-map)
                 (bind-key "M-u"           'paxedit-previous-symbol   paxedit-mode-map)
                 (bind-key "M-<backspace>" 'paxedit-kill              paxedit-mode-map)))

(use-package clojure-mode
  :config (define-clojure-indent
            (defroutes 'defun)
            (fnk 'defun)
            (defnk 'defun)
            (GET 2)
            (POST 2)
            (PUT 2)
            (DELETE 2)
            (HEAD 2)
            (ANY 2)
            (context 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package prelude
  :config
  (progn
    (setq pcache-directory (f-expand "pcache" prelude-savefile-dir))
    (unbind-key "M-o" prelude-mode-map)))

(use-package magit
  :config
  (progn
    (mapc (apply-partially 'add-to-list 'magit-repo-dirs)
          (f-directories user-projects-directory))
            
    (setq magit-status-buffer-switch-function 'switch-to-buffer)
    (setq magit-completing-read-function 'ido-completing-read)
    (setq magit-repo-dirs (list user-projects-directory))
    (setq magit-set-upstream-on-push 'dontask)
    (setq magit-save-some-buffers 'dontask)

    ;; Make magit restore the original window configuration when you leave the
    ;; magit buffer.
    ;;
    ;; http://whattheemacsd.com/setup-magit.el-01.html
    ;; http://irreal.org/blog/?p=2253
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))
           
    (bind-key "q" 'magit-quit-session magit-status-mode-map)))

(use-package org
  :config
  (progn
    (setq org-src-fontify-natively t)
    (setq org-hide-leading-stars t)
    (after 'prelude-mode (add-hook 'org-mode-hook 'prelude-off))))

(use-package org-capture
  :bind ("C-c c" . org-capture)
  
  :config
  (progn
    (defvar user-org-capture-directory
      (f-join user-dropbox-directory "Documents" "org")
      "The user's org-mode capture directory.")
    (setq org-default-notes-file (f-join user-org-capture-directory "capture.org"))
            
    ;; http://orgmode.org/manual/Template-elements.html
    (setq org-capture-templates
          '(("l" "Life Log" entry (file+datetree+prompt (f-join user-org-capture-directory "log.org"))
             ("%?"))
            ("m" "Music" entry (file (f-join user-org-capture-directory "music.org"))
             "* TODO %?\n")
            ("r" "Reading" entry (file (f-join user-org-capture-directory ".org"))
             "* TODO %?\n")))))

(use-package markdown-mode
  :config (when (executable-find "gfm") (setq markdown-command "gfm")))

(use-package golden-ratio
  ;; https://github.com/roman/golden-ratio.el/issues/25
  :config (setq window-combination-resize t))

(use-package js
  :bind (("," . self-insert-command)
         ("RET" . reindent-then-newline-and-indent))
  
  :config (setq js-indent-level 2))

(use-package js2-mode-hook
  :config
  (progn
    (setq js2-basic-offset 2)
    (setq js2-include-node-externs t)
    (setq js2-include-browser-externs t)))

(use-package cider
  :bind (("C-j" . nil)
         ("S-<return>" . cider-repl-newline-and-indent)
         ("C-c M-r" . cider-refresh))
  
  :config
  (progn
    (setq cider-repl-history-file (f-expand "cider-repl-history" prelude-savefile-dir))
    (setq cider-repl-use-clojure-font-lock t)
    (setq cider-repl-use-pretty-printing t)
    (setq cider-repl-popup-stacktraces t)
    (setq cider-auto-select-error-buffer nil)
    (setq nrepl-hide-special-buffers nil)
    (setq cider-repl-print-length 100)

    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode)
    (after 'prelude-mode (add-hook 'cider-repl-mode-hook 'prelude-off))
    (after 'rainbow-delimiters (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))))

(use-package windmove
  :bind (("H-i" . windmove-up)
         ("C-j" . windmove-left)
         ("C-k" . windmove-down)
         ("C-l" . windmove-right)))

(use-package company
  :diminish company-mode
  
  :config
  (progn
    (bind-key "M-k" 'company-select-next-or-abort company-active-map)
    (bind-key "M-i" 'company-select-previous-or-abort company-active-map)
    (bind-key "C-f" 'company-search-candidates company-active-map)
    (bind-key "C-M-f" 'company-filter-candidates company-active-map)
    (bind-key "C-c C-d" 'company-show-doc-buffer company-active-map)

    (setq company-idle-delay 0.2)

    (add-hook 'prog-mode-hook 'company-mode-on)
    (add-hook 'cider-repl-mode-hook 'company-mode-on)

    (setq company-show-numbers t)))

(use-package web-mode)

(use-package sql-indent
  :idle (load-library "sql-indent")
  :config
  (setq sql-indent-first-column-regexp
        (concat "^\\s-*" (regexp-opt '("select" "update" "insert" "delete"
                                       "union" "intersect"
                                       "from" "where" "into" "group" "having" "order"
                                       "set"
                                       "create" "drop" "truncate"
                                       "limit"
                                       "--") t) "\\(\\b\\|\\s-\\)")))

(use-package fancy-narrow
  :disabled t
  :idle (progn (require 'fancy-narrow)
               (fancy-narrow-mode t)))

(use-package undo-tree
  :bind (("M-z" . undo-tree-undo)
         ("M-Z" . undo-tree-redo)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-;"         . mc/mark-all-dwim))
  :config (progn (setq mc/list-file (f-expand ".mc-lists.el" prelude-savefile-dir))
                 (bind-key "C-;" 'mc/mark-all-symbols-like-this-in-defun prog-mode-map)))

(use-package expand-region
  :bind (("M->" . er/expand-region)
         ("M-<" . er/contract-region)))

(use-package key-chord
  :config (key-chord-define-global "lj" nil))

(use-package smex
  :bind (("M-a" . smex)
         ("M-A" . smex-major-mode-commands)
         ("C-c C-c M-a" . execute-extended-command)))

(use-package ido-vertical-mode
  :commands ido-vertical-mode
  :init (ido-vertical-mode +1)
  
  :config
  (progn
    (bind-key "M-k" 'ido-next-match     ido-completion-map)
    (bind-key "M-i" 'ido-prev-match     ido-completion-map)
    (bind-key "M-j" 'ido-prev-match-dir ido-completion-map)
    (bind-key "M-l" 'ido-next-match-dir ido-completion-map)))

(use-package vlf
  :idle (require 'vlf-integrate))

(when window-system
  (global-hl-line-mode -1)
  (add-hook 'prog-mode-hook 'hl-line-mode))

(use-package projectile
  :diminish projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
(keyboard-translate ?\C-i ?\H-i)

(bind-key "M-i" 'previous-line)
(bind-key "M-j" 'backward-char)
(bind-key "M-k" 'next-line)
(bind-key "M-l" 'forward-char)

(bind-key "M-u" 'backward-word)
(bind-key "M-o" 'forward-word)

(bind-key "M-n" 'beginning-of-buffer)
(bind-key "M-N" 'end-of-buffer)

(bind-key "M-I" 'scroll-down)
(bind-key "M-K" 'scroll-up)

(bind-key "M-h" 'beginning-of-line)
(bind-key "M-H" 'end-of-line)

(bind-key "M-f" 'delete-char)
(bind-key "M-d" 'delete-backward-char)

(defun new-empty-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "Untitled")))

(bind-key "C-a" 'mark-whole-buffer)
(bind-key "C-n" 'new-empty-buffer)
(bind-key "C-w" 'kill-this-buffer)
(bind-key "C-s" 'save-buffer)

(bind-key "M-y" 'isearch-forward)
(bind-key "M-Y" 'isearch-backward)

(bind-key "M-2" 'delete-window)
(bind-key "M-3" 'delete-other-windows)
(bind-key "M-4" 'split-window-vertically)
(bind-key "M-$" 'split-window-horizontally)

(bind-key "M-c" 'clipboard-kill-ring-save)
(bind-key "M-x" 'clipboard-kill-region)
(bind-key "M-v" 'clipboard-yank)
(bind-key "M-z" 'undo)

(bind-key "M-p" 'recenter-top-bottom)

(bind-key "C-s" 'save-buffer)

(bind-key "C-x y" 'bury-buffer)
(bind-key "C-x b" 'ido-switch-buffer)

(when (not window-system)
  (bind-key "C-@" 'set-mark-command))

(provide 'personal)
;;; personal.el ends here
