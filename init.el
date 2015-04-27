;;; personal --- Summary
;;;
;;; Commentary:

;; https://github.com/technomancy/nrepl-discover
;; https://github.com/magnars/js2-refactor.el
;; TODO: http://www.wisdomandwonder.com/article/8105/a-reminder-for-a-library-config
;; TODO: https://github.com/bartman/git-wip

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(package-install 'use-package)
(require 'use-package)

(use-package f :ensure t)

(defvar user-home-directory
  (f-expand ".." user-emacs-directory)
  "The user's home directory.")

(defvar user-projects-directory
  (f-join user-home-directory "Projects")
  "The user's projects directory.")

(defvar user-dropbox-directory
  (f-join "Dropbox" user-home-directory)
  "The user's Dropbox directory.")

(defvar savefile-dir
  (f-join user-emacs-directory "savefile"))
(unless (file-exists-p savefile-dir) (make-directory savefile-dir))

(push (f-join user-emacs-directory "lisp") load-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yanked from prelude
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; .............................................................................
;; Editor

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(use-package saveplace :ensure t ; remembers your location in a file when saving files
  :idle (progn
          (require 'saveplace)
          (setq-default save-place t))
  :config (setq save-place-file (expand-file-name "saveplace" savefile-dir)))

(use-package savehist                   ; keeps track of some history
  :commands savehist-mode
  :idle (progn (savehist-mode +1))
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir)))


(use-package recentf                    ; save recent files
  :commands recentf-mode
  :idle (recentf-mode +1)
  :config
  (progn
    (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
          recentf-max-saved-items 500
          recentf-max-menu-items 15
          ;; disable recentf-cleanup on Emacs start, because it can cause
          ;; problems with remote files
          recentf-auto-cleanup 'never)
    
    (defun recentf-exclude-p (file)
      "A predicate to decide whether to exclude FILE from recentf."
      (let ((file-dir (file-truename (file-name-directory file))))
        (-any-p (lambda (dir)
                  (string-prefix-p dir file-dir))
                (mapcar 'file-truename (list savefile-dir package-user-dir)))))

    (add-to-list 'recentf-exclude 'recentf-exclude-p)
    ;; ignore magit's commit message files
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defvar should-auto-save t)
(defun auto-save-command ()
  "Save the current buffer if `should-auto-save' is not nil."
  (when (and should-auto-save
             buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))
(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))
;; advise all window switching functions
(advise-commands
 "auto-save"
 (switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right)
 before
 (auto-save-command))
(add-hook 'mouse-leave-buffer-hook 'auto-save-command)
(when (version<= "24.4" emacs-version) (add-hook 'focus-out-hook 'auto-save-command))

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (with-current-buffer buffer (if mode (funcall mode)))))

(use-package hl-line
  :config (add-hook 'prog-mode-hook 'hl-line-mode))

(global-hl-line-mode +1)

(use-package volatile-highlights
  :commands volatile-highlights-mode
  :idle (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

(use-package tramp
  ;; keep in mind known issues with zsh - see emacs wiki
  :config (setq tramp-default-method "ssh"))

(set-default 'imenu-auto-rescan t)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(defun prelude-enable-flyspell ()
  "Enable command `flyspell-mode' if `prelude-flyspell' is not nil."
  (when (and prelude-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

(add-hook 'text-mode-hook 'prelude-enable-flyspell)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
        bookmark-save-flag 1))

(use-package anzu :ensure t ; enhances isearch & query-replace by showing total matches and current match position
  :ensure anzu
  :diminish anzu-mode
  :idle (global-anzu-mode))

;; shorter aliases for ack-and-a-half commands
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(use-package dired
  :commands dired
  :config
  (progn
    ;; dired - reuse current buffer by pressing 'a'
    (put 'dired-find-alternate-file 'disabled nil)
    
    ;; always delete and copy recursively
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always)

    ;; enable some really cool extensions like C-x C-j(dired-jump)
    (use-package dired-x)))

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

(use-package ediff
  :commands ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain)) ; ediff - don't start another frame

(use-package midnight) ; clean up obsolete buffers automatically

(use-package browse-kill-ring ; smarter kill-ring navigation
  :bind ("s-y" . browse-kill-ring)
  :config (browse-kill-ring-default-keybindings))

;; (defadvice exchange-point-and-mark (before deactivate-mark activate compile)
;;   "When called with no active region, do not activate mark."
;;   (interactive
;;    (list (not (region-active-p)))))

;; (defmacro with-region-or-buffer (func)
;;   "When called with no active region, call FUNC on current buffer."
;;   `(defadvice ,func (before with-region-or-buffer activate compile)
;;      (interactive
;;       (if mark-active
;;           (list (region-beginning) (region-end))
;;         (list (point-min) (point-max))))))

;; (with-region-or-buffer indent-region)
;; (with-region-or-buffer untabify)

;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) prelude-yank-indent-threshold)
      (indent-region beg end nil)))

(advise-commands "indent" (yank yank-pop) after
  "If current mode is one of `prelude-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (not (member major-mode prelude-indent-sensitive-modes))
           (or (derived-mode-p 'prog-mode)
               (member major-mode prelude-yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; saner regex syntax
(use-package re-builder
  :config (setq reb-re-syntax 'string))


(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" savefile-dir))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" savefile-dir))

;; Compilation from Emacs
(defun prelude-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'prelude-colorize-compilation-buffer)

(use-package winner
  :ensure winner
  :idle (winner-mode +1))

(use-package diff-hl :ensure diff-hl
  :commands global-diff-hl-mode
  :idle (global-diff-hl-mode +1))

(use-package diff-hl-dired :ensure diff-hl
  :disabled t
  :commands diff-hl-dired-mode
  :init (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

;; easy-kill
(use-package easy-kill
  :ensure easy-kill
  :commands (easy-kill easy-mark)
  
  :init
  (progn
    (global-set-key [remap kill-ring-save] 'easy-kill)
    (global-set-key [remap mark-sexp] 'easy-mark)))

;; .............................................................................
;; UI

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(menu-bar-mode -1)
(blink-cursor-mode -1) ;; the blinking cursor is nothing, but an annoyance
(setq inhibit-startup-screen t) ;; disable startup screen

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller the argument is a width in pixels (the default is 8)
(when (fboundp 'fringe-mode) (fringe-mode 4))

(fset 'yes-or-no-p 'y-or-n-p) ;; enable y/n answers

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Emacs - " (:eval (if (buffer-file-name)
						  (abbreviate-file-name (buffer-file-name))
						"%b"))))

(use-package ov :ensure ov)
(use-package volatile-highlights :ensure volatile-highlights)
(use-package anzu :ensure anzu)
(use-package browse-kill-ring :ensure browse-kill-ring)

(use-package prelude
  :disabled t
  :config
  (progn
    (setq pcache-directory (f-expand "pcache" savefile-dir))
    (unbind-key "M-o" prelude-mode-map)))

;; .............................................................................
;; Mac

(defun z:mac-p ()
  "Truthy if the host OS is a Mac."
  (string-match "apple-darwin" system-configuration))

(defvar mac-system
  (z:mac-p)
  "Truthy if the host OS is a Mac.")

(when (z:mac-p)
  (menu-bar-mode +1)
  
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)

  (bind-key* "M-`" 'ns-next-frame))

(use-package exec-path-from-shell :ensure t
  :if mac-system
  :init (exec-path-from-shell-initialize))

(use-package vkill                ; proced-mode doesn't work on OS X
    :if mac-system
    :disabled t
    :commands vkill
    :bind ("C-x p" . vkill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions and Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions and Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun increment-number-at-point ()
  "Increments the number at point."
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

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
  (interactive "Press key: ")
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

(use-package smart-mode-line :ensure t
  :if window-system
  :commands (sml/setup sml/apply-theme)
  
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (sml/apply-theme 'dark)
    (sml/setup))
  
  :config
  (add-to-list 'sml/replacer-regexp-list '("^~/Projects/" ":P:")))

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
  (scroll-bar-mode -1)
  
  (defun text-scale-default () (interactive) (text-scale-set 0))
  (bind-key "M-+" 'text-scale-increase)
  (bind-key "M--" 'text-scale-decrease)
  (bind-key "M-0" 'text-scale-default))

(use-package paren
  :config
  (progn
    (set-face-background 'show-paren-match nil)
    (set-face-foreground 'show-paren-match nil)
    (set-face-inverse-video-p 'show-paren-match nil)))

(use-package diminish
  :init
  (progn
    (defmacro rename-modeline (package-name mode new-name)
      `(eval-after-load ,package-name
         '(defadvice ,mode (after rename-modeline activate)
            (setq mode-name ,new-name))))

    ;; http://whattheemacsd.com//appearance.el-01.html
    (rename-modeline 'lisp-mode emacs-lisp-mode "EL")
    (rename-modeline 'lisp-mode lisp-interaction-mode "LI")
    (rename-modeline 'js js-mode "JS")))

(defun turn-on-electric-indent-mode ()
  "Enables electric indent mode."
  (electric-indent-mode +1))

(add-hook 'emacs-lisp-mode-hook 'turn-on-electric-indent-mode)
(add-hook 'prog-mode-hook 'turn-on-electric-indent-mode)

(bind-key "C-o"
          (lambda () (interactive)
            (let ((case-fold-search isearch-case-fold-search))
              (occur (if isearch-regexp
                         isearch-string
                       (regexp-quote isearch-string)))))
          isearch-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :config
  (progn
    (setq flycheck-mode-line-lighter " fl")
    (set-face-attribute 'flycheck-error nil :underline "red")
    (set-face-attribute 'flycheck-warning nil :underline "yellow")))

(use-package smartparens
  :commands (smartparens-global-mode show-smartparens-global-mode)
  :init
  (progn
    (smartparens-global-mode)
    (show-smartparens-global-mode +1))
  
  :config
  (progn
    (require 'smartparens-config)

    (sp-use-paredit-bindings)
    (setq sp-base-key-bindings 'paredit)
    (setq sp-autoskip-closing-pair 'always)
    (setq sp-hybrid-kill-entire-symbol t)
    (setq sp-hybrid-kill-excessive-whitespace nil)
    (setq sp-autoinsert-if-followed-by-word t)
    (setq sp-navigate-close-if-unbalanced t)
    
    ;; https://github.com/Fuco1/smartparens/wiki/Paredit-and-smartparens#random-differences
    (unbind-key "M-r" smartparens-mode-map)
    (bind-keys :map smartparens-mode-map
               (")"   . sp-up-sexp)

               ("M-d" . sp-backward-delete-char)
               ("M-f" . sp-delete-char)
               
               ("M-e" . sp-backward-kill-word)
               ("M-r" . sp-kill-word)
               
               ("M-D" . sp-backward-kill-sexp)
               ("M-F" . sp-kill-sexp)
               
               ("M-g" . sp-kill-hybrid-sexp))

    (defun wrap-with (s)
      "Create a wrapper function for smartparens using S."
      `(lambda (&optional arg)
         (interactive "P")
         (sp-wrap-with-pair ,s)))

    (bind-key "M-(" (wrap-with "(") prog-mode-map)
    (bind-key "M-\"" (wrap-with "\"") prog-mode-map)))

(use-package paredit :ensure t)

(use-package paxedit :ensure t
  :diminish "pax"
  :commands paxedit-mode
  
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paxedit-mode)
    (add-hook 'clojure-mode-hook 'paxedit-mode))
  
  :config
  (progn
    (bind-keys :map paxedit-mode-map
               ("M-w"           . paxedit-delete-whitespace)
               ("s-i"           . paxedit-backward-up)
               ("s-k"           . paxedit-backward-end)
               ("s-l"           . paxedit-next-symbol)
               ("s-j"           . paxedit-previous-symbol)
               ("M-<backspace>" . paxedit-kill))))

(use-package clojure-mode
  :config
  (define-clojure-indent
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

(use-package magit :ensure t
  :bind ("C-x g" . magit-status)
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
           
    (bind-key "q" 'magit-quit-session magit-status-mode-map)
    (unbind-key "M-p" magit-status-mode-map)))

(use-package org :ensure t
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
  :disabled t
  :config
  (progn
    (setq js2-basic-offset 2)
    (setq js2-include-node-externs t)
    (setq js2-include-browser-externs t)))

(use-package cider :ensure t
  :commands cider-jack-in
  :bind (("S-<return>" . cider-repl-newline-and-indent)
         ("C-c M-r" . cider-refresh))
  
  :config
  (progn
    (unbind-key "C-j" cider-repl-mode-map)
    
    (setq cider-repl-history-file (f-join savefile-dir "cider-repl-history"))
    (setq cider-repl-use-clojure-font-lock t)
    (setq cider-repl-use-pretty-printing t)
    (setq cider-repl-popup-stacktraces t)
    (setq cider-auto-select-error-buffer nil)
    (setq nrepl-hide-special-buffers nil)
    (setq cider-repl-print-length 100)

    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode)))

(use-package solarized-theme
  :if window-system
  :pre-load
  (progn
    (setq solarized-use-variable-pitch nil)

    (setq solarized-scale-org-headlines nil)

    ;; Avoid all font-size changes
    (setq solarized-height-minus-1 1)
    (setq solarized-height-plus-1 1)
    (setq solarized-height-plus-2 1)
    (setq solarized-height-plus-3 1)
    (setq solarized-height-plus-4 1)
    
    (setq solarized-distinct-fringe-background t)
    (setq solarized-high-contrast-mode-line nil))
  :init
  (load-theme 'solarized-dark t))

(use-package whitespace
  :if window-system
  :idle (global-whitespace-mode)
  :config
  (progn
    ;; http://xahlee.blogspot.com/2009/08/how-to-use-and-setup-emacss-whitespace.html
    (setq whitespace-trailing-regexp "^.*[^\r\n\t \xA0\x8A0\x920\xE20\xF20]+\\([\t \xA0\x8A0\x920\xE20\xF20]+\\)$")
    (setq whitespace-line-column 100)
    (setq whitespace-action '(auto-cleanup warn-if-read-only))
    (setq whitespace-style '(face tabs empty trailing))

    (defun prelude-cleanup-maybe ()
      "Invoke `whitespace-cleanup' if `prelude-clean-whitespace-on-save' is not nil."
      (when prelude-clean-whitespace-on-save
        (whitespace-cleanup)))

    (defun prelude-enable-whitespace ()
      "Enable `whitespace-mode' if `prelude-whitespace' is not nil."
      (when prelude-whitespace
        ;; keep the whitespace decent all the time (in this buffer)
        (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)
        (whitespace-mode +1)))
    (add-hook 'text-mode-hook 'prelude-enable-whitespace)))

(use-package rainbow-delimiters :ensure t
  :commands rainbow-delimiters-mode-enable
  :config
  (progn
    (setq-default frame-background-mode 'dark)
    (let ((hooks '(emacs-lisp-mode-hook
                   clojure-mode-hook
                   js-mode-hook
                   lisp-mode-hook
                   python-mode-hook)))
      (dolist (hook hooks)
        (add-hook hook 'rainbow-delimiters-mode-enable)))
    (after 'elisp-mode (add-hook 'emacs-lisp-mode-hook  'rainbow-delimiters-mode-enable))
    (after 'clojure    (add-hook 'clojure-mode-hook     'rainbow-delimiters-mode-enable))
    (after 'nrepl      (add-hook 'nrepl-mode-hook       'rainbow-delimiters-mode-enable))
    (after 'cider      (add-hook 'cider-repl-mode-hook  'rainbow-delimiters-mode-enable))))

(use-package rainbow-identifiers :ensure t
  :commands rainbow-identifiers-mode
  :init (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

(use-package windmove :ensure t
  :config
  (progn
    (bind-key* "H-i" 'windmove-up)
    (bind-key* "C-j" 'windmove-left)
    (bind-key* "C-k" 'windmove-down)
    (bind-key* "C-l" 'windmove-right)))

(use-package framemove :ensure t
  :config
  (setq framemove-hook-into-windmove t))

(use-package company :ensure t
  :diminish company-mode
  :commands company-mode-on
  
  :config
  (progn
    (bind-key "M-k"     'company-select-next-or-abort     company-active-map)
    (bind-key "M-i"     'company-select-previous-or-abort company-active-map)
    (bind-key "C-f"     'company-search-candidates        company-active-map)
    (bind-key "C-M-f"   'company-filter-candidates        company-active-map)
    (bind-key "C-c C-d" 'company-show-doc-buffer          company-active-map)

    (setq company-idle-delay 0.2)

    (add-hook 'prog-mode-hook 'company-mode-on)
    (add-hook 'cider-repl-mode-hook 'company-mode-on)

    (setq company-show-numbers t)))

(use-package web-mode :ensure t)

(use-package sql-indent :ensure t
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

(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :bind (("M-z" . undo-tree-undo)
         ("M-Z" . undo-tree-redo)))

(use-package multiple-cursors :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-;"         . mc/mark-all-dwim))
  :config (progn (setq mc/list-file (f-expand ".mc-lists.el" savefile-dir))
                 (bind-key "C-;" 'mc/mark-all-symbols-like-this-in-defun prog-mode-map)))

(use-package expand-region :ensure t
  :bind (("M->" . er/expand-region)
         ("M-<" . er/contract-region)))

(use-package key-chord
  :disabled t)

(use-package ido :ensure t
  :init (ido-mode +1)
  :config
  (progn
    (setq ido-case-fold t
          ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-max-prospects 10
          ido-save-directory-list-file (expand-file-name "ido.hist" savefile-dir)
          ido-default-file-method 'selected-window
          ido-auto-merge-work-directories-length -1)

    (setq ido-ignore-buffers '("\\` "))
    
    (setq ido-file-extensions-order
          '(".org"
            ".txt"
            ".clj"
            ".py"
            ".emacs"
            ".xml"
            ".el"
            ".ini"
            ".cfg"
            ".cnf"
            ".gz"))))

(use-package ido-ubiquitous :ensure t
  :init
  (progn
    (ido-mode +1)
    (ido-ubiquitous-mode +1)))

(use-package flx-ido :ensure t        ; smarter fuzzy matching for ido
  :init (flx-ido-mode +1)

  ;; disable ido faces to see flx highlights
  :config (setq ido-use-faces nil))

(use-package ido-sort-mtime :ensure t
  :init (ido-sort-mtime-mode 1)
  :config (setq ido-sort-mtime-tramp-files-at-end t))

(use-package smex :ensure t
  :bind (("M-a" . smex)
         ("M-A" . smex-major-mode-commands)
         ("C-c C-c M-a" . execute-extended-command))
  :init
  (smex-initialize)
  
  :config
  (progn
    (bind-key* "M-a" 'smex)
    (setq smex-save-file (f-join savefile-dir ".smex-items"))))

(use-package ido-vertical-mode :ensure t
  :commands ido-vertical-mode
  :init (after 'ido (ido-vertical-mode +1))
  
  :config
  (progn
    (defun ido-vertical-define-my-keys ()
      (define-key ido-completion-map (kbd "M-k") 'ido-next-match)
      (define-key ido-completion-map (kbd "M-i") 'ido-prev-match))
    (setq ido-setup-hook '(ido-vertical-define-my-keys))
    (add-hook 'ido-minibuffer-setup-hook 'ido-vertical-define-my-keys)))

(use-package projectile :ensure t
   :commands projectile-global-mode

   :init
   (setq projectile-known-projects-file (f-join savefile-dir "projectile-bookmarks.eld")
         projectile-cache-file          (f-join savefile-dir "projectile.cache"))
   (projectile-global-mode t)
   
   :diminish projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
(keyboard-translate ?\C-i ?\H-i)

(unbind-key "M-`")

(bind-key "M-i" 'previous-line)
(bind-key "M-j" 'backward-char)
(bind-key "M-k" 'next-line)
(bind-key "M-l" 'forward-char)

(bind-key "M-e" 'backward-kill-word)

(bind-key "M-u" 'backward-word)
(bind-key "M-o" 'forward-word)
(bind-key "M-U" 'backward-paragraph)
(bind-key "M-O" 'forward-paragraph)

(bind-key "M-n" 'beginning-of-buffer)
(bind-key "M-N" 'end-of-buffer)

(bind-key "M-I" 'scroll-down)
(bind-key "M-K" 'scroll-up)

(bind-key* "M-h" 'beginning-of-line)
(bind-key "M-H" 'end-of-line)

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(bind-key "M-f" 'delete-char)
(bind-key "M-d" 'delete-backward-char)
(bind-key "M-g" 'kill-line)
(bind-key "M-G" 'backward-kill-line)
(bind-key "M-e" 'backward-kill-word)
(bind-key "M-r" 'kill-word)

(defun new-empty-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "Untitled")))

(bind-key* "C-a" 'mark-whole-buffer)
(bind-key "C-n" 'new-empty-buffer)
(bind-key "C-w" 'kill-this-buffer)
(bind-key "C-s" 'save-buffer)
(bind-key "C-o" 'ido-find-file)

(bind-key "M-y" 'isearch-forward)
(bind-key "M-Y" 'isearch-backward)
(bind-key "M-y" 'isearch-repeat-forward isearch-mode-map)
(bind-key "M-Y" 'isearch-repeat-backward isearch-mode-map)

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

(use-package helm :ensure t
  :config
  (progn
    ;; http://tuhdo.github.io/helm-intro.html
    
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (bind-key (kbd "C-c h") 'helm-command-prefix)
    (unbind-key (kbd "C-x c"))
    
    (when (executable-find "ack-grep")
      (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
            helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-ff-file-name-history-use-recentf t)
    (bind-keys :map helm-map
               ("M-i" . helm-previous-line)
               ("M-k" . helm-next-line)
                    
               ("M-I" . helm-previous-line)
               ("M-K" . helm-next-page)

               ("M-n" . helm-beginning-of-buffer)
               ("M-N" . helm-end-of-buffer)

               ("M-O" . helm-next-source)
               ("M-U" . helm-previous-source))))

(use-package helm-misc :ensure helm
  :bind ("C-x b" . helm-mini))

(use-package helm-command :ensure helm
  :bind ("M-a" . helm-M-x))

(use-package helm-files :ensure helm
  :bind ("C-o" . helm-find-files)
  :config (bind-key "M-i" 'helm-previous-line helm-find-files-map))

(use-package helm-projectile :ensure t
  :init (helm-projectile-toggle +1))

(use-package zoom-frm ; vendored in the `lisp' subdirectory
  :pre-load (use-package frame-cmds)
  :if window-system
  :bind (("M-+" . zoom-frm-in)
         ("M--" . zoom-frm-out)
         ("M-0" . zoom-frm-unzoom)))

(use-package swiper :ensure t
  :init (progn (bind-key "M-y" 'swiper)
               (bind-key "M-Y" 'swiper)))

(use-package ivy
  :config
  (progn
    (bind-key "C-m" 'ivy-done ivy-minibuffer-map)
    (bind-key "C-j" 'ivy-alt-done ivy-minibuffer-map)
    (bind-key "M-k" 'ivy-next-line ivy-minibuffer-map)
    (bind-key "M-i" 'ivy-previous-line ivy-minibuffer-map)
    (bind-key "M-d" 'ivy-backward-delete-char ivy-minibuffer-map)
    (bind-key "M-n" 'ivy-beginning-of-buffer ivy-minibuffer-map)
    (bind-key "M-N" 'ivy-end-of-buffer ivy-minibuffer-map)
    (bind-key "M-K" 'ivy-scroll-up-command ivy-minibuffer-map)
    (bind-key "M-I" 'ivy-scroll-down-command ivy-minibuffer-map)
    ;; See `ivy-minibuffer-map' for more bindable keys.
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'personal)
;;; personal.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
