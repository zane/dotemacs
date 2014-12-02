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
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(package-install 'use-package)
(require 'use-package)

(use-package f
  :ensure f
  :init (require 'f))

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

;; diminish keeps the modeline tidy
(require 'diminish)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
;; activate it for all buffers
(setq-default save-place t)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" savefile-dir))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(defun prelude-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (-any-p (lambda (dir)
              (string-prefix-p dir file-dir))
            (mapcar 'file-truename (list savefile-dir package-user-dir)))))

(add-to-list 'recentf-exclude 'prelude-recentf-exclude-p)
;; ignore magit's commit message files
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")

(recentf-mode +1)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defvar should-auto-save t)
(defun auto-save-command ()
  "Save the current buffer if `prelude-auto-save' is not nil."
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
(add-hook 'mouse-leave-buffer-hook 'prelude-auto-save-command)
(when (version<= "24.4" emacs-version) (add-hook 'focus-out-hook 'auto-save-command))

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (with-current-buffer buffer (if mode (funcall mode)))))

(use-package hl-line
  :config (add-hook 'prog-mode-hook 'hl-line-mode))

(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

(set-default 'imenu-auto-rescan t)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(defun prelude-enable-flyspell ()
  "Enable command `flyspell-mode' if `prelude-flyspell' is not nil."
  (when (and prelude-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

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

(add-hook 'text-mode-hook 'prelude-enable-flyspell)
(add-hook 'text-mode-hook 'prelude-enable-whitespace)

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

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
(use-package anzu
  :ensure anzu
  :diminish anzu 
  :idle (global-anzu-mode))

;; shorter aliases for ack-and-a-half commands
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)

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

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

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

(use-package diff-hl
  :ensure diff-hl
  :commands diff-hl-dired-mode
  :idle (global-diff-hl-mode +1)
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
  :config
  (progn
    (setq pcache-directory (f-expand "pcache" savefile-dir))
    (unbind-key "M-o" prelude-mode-map)))

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

(when (z:mac-p)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(use-package solarized-theme
  :if window-system
  :init (load-theme 'solarized-dark t)
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

(use-package smart-mode-line
  :if window-system
  :commands sml/setup
  
  :init
  (progn 
    (setq sml/no-confirm-load-theme t)
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
  (rename-modeline 'js js-mode "JS"))

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

;; smart pairing for all
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)

(use-package smartparens
  :init (smartparens-global-mode)
  
  :config
  (progn
    (setq sp-base-key-bindings 'paredit)
    (sp-use-paredit-bindings)
    (setq sp-autoinsert-if-followed-by-word t)
    (setq sp-navigate-close-if-unbalanced t)
    
    ;; https://github.com/Fuco1/smartparens/wiki/Paredit-and-smartparens#random-differences
    (bind-key ")"   'sp-up-sexp            smartparens-mode-map)
    (bind-key "M-F" 'sp-kill-sexp          smartparens-mode-map)
    (bind-key "M-D" 'sp-backward-kill-sexp smartparens-mode-map)

    (defun wrap-with (s)
      "Create a wrapper function for smartparens using S."
      `(lambda (&optional arg)
         (interactive "P")
         (sp-wrap-with-pair ,s)))

    (bind-key "M-(" (wrap-with "(") prog-mode-map)
    (bind-key "M-\"" (wrap-with "\"") prog-mode-map)))

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

(use-package magit
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
  :bind (("S-<return>" . cider-repl-newline-and-indent)
         ("C-c M-r" . cider-refresh))
  
  :config
  (progn
    (unbind-key "C-j" cider-repl-mode-map)
    
    (setq cider-repl-history-file (f-expand "cider-repl-history" savefile-dir))
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
  :init (global-undo-tree-mode)
  :bind (("M-z" . undo-tree-undo)
         ("M-Z" . undo-tree-redo)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-;"         . mc/mark-all-dwim))
  :config (progn (setq mc/list-file (f-expand ".mc-lists.el" savefile-dir))
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
  :init (after 'ido (ido-vertical-mode +1))
  
  :config
  (add-hook
   ido-setup-hook
   (lambda ()
     (bind-key "M-k" 'ido-next-match     ido-completion-map)
     (bind-key "M-i" 'ido-prev-match     ido-completion-map)
     (bind-key "M-j" 'ido-prev-match-dir ido-completion-map)
     (bind-key "M-l" 'ido-next-match-dir ido-completion-map))))

(use-package vlf
  :idle (require 'vlf-integrate))

 (use-package projectile
   :commands projectile-global-mode
   :init (projectile-global-mode t)
   :config
   (setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
   :diminish projectile-kmode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
(keyboard-translate ?\C-i ?\H-i)

(bind-key "M-i" 'previous-line)
(bind-key "M-j" 'backward-char)
(bind-key "M-k" 'next-line)
(bind-key "M-l" 'forward-char)

(bind-key "M-e" 'backward-kill-word)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
