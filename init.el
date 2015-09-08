;;; personal --- Summary
;;;
;;; Commentary:

;; https://github.com/technomancy/nrepl-discover
;; https://github.com/magnars/js2-refactor.el
;; TODO: http://www.wisdomandwonder.com/article/8105/a-reminder-for-a-library-config
;; TODO: https://github.com/bartman/git-wip
;;
;; http://seancribbs.com/emacs.d/
;; http://www.aaronbedra.com/emacs.d/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://whattheemacsd.com/init.el-01.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off mouse interface early in startup to avoid momentary display
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (f-full (f-join user-home-directory "Projects"))
  "The user's projects directory.")

(defvar user-dropbox-directory
  (f-full (f-join "Dropbox" user-home-directory "Dropbox"))
  "The user's Dropbox directory.")

(defvar savefile-dir
  (f-full (f-join user-emacs-directory "savefile")))
(unless (file-exists-p savefile-dir) (make-directory savefile-dir))

(push (f-full (f-join user-emacs-directory "lisp")) load-path)

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

(transient-mark-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
(keyboard-translate ?\C-i ?\H-i)

(unbind-key "M-`")

(bind-key "M-a" 'execute-extended-command)

(bind-key "M-i" 'previous-line)
(bind-key "M-j" 'backward-char)
(bind-key "M-k" 'next-line)
(bind-key "M-l" 'forward-char)

(bind-key* "M-e" 'backward-kill-word)

(bind-key "M-u" 'backward-word)
(bind-key "M-o" 'forward-word)
(bind-key "M-U" 'backward-paragraph)
(bind-key "M-O" 'forward-paragraph)

(bind-key* "M-n" 'beginning-of-buffer)
(bind-key "M-N" 'end-of-buffer)

(bind-key "M-I" 'scroll-down)
(bind-key "M-K" 'scroll-up)

(bind-key* "M-h" 'beginning-of-line)
(bind-key* "M-H" 'end-of-line)

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
(bind-key "C-o" 'find-file)

(bind-key "M-y" 'isearch-forward)
(bind-key "M-Y" 'isearch-backward)
(bind-key "M-y" 'isearch-repeat-forward isearch-mode-map)
(bind-key "M-Y" 'isearch-repeat-backward isearch-mode-map)

(bind-key* "M-2" 'delete-window)
(bind-key* "M-3" 'delete-other-windows)
(bind-key* "M-4" 'split-window-vertically)
(bind-key* "M-$" 'split-window-horizontally)

(bind-key "M-c" 'clipboard-kill-ring-save)
(bind-key "M-x" 'clipboard-kill-region)
(bind-key* "M-v" 'clipboard-yank)
(bind-key "M-z" 'undo)

(bind-key* "M-p" 'recenter-top-bottom)

(bind-key "C-s" 'save-buffer)

(bind-key "C-x y" 'bury-buffer)
(bind-key "C-x b" 'switch-to-buffer)

(when (not window-system)
  (bind-key "C-@" 'set-mark-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun z:mac-p ()
  "Truthy if the host OS is a Mac."
  (string-match "apple-darwin" system-configuration))

(defvar mac-system
  (z:mac-p)
  "Truthy if the host OS is a Mac.")

(when (z:mac-p)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)

  (bind-key* "M-`" 'ns-next-frame))

(use-package exec-path-from-shell :ensure t
  :if mac-system
  :init (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions and Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://milkbox.net/note/single-file-master-emacs-configuration/
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

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

(blink-cursor-mode -1)

;; ;; nice scrolling
(setq scroll-margin 0)
;(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; ;; make the fringe (gutter) smaller the argument is a width in pixels (the default is 8)
(when (fboundp 'fringe-mode) (fringe-mode 4))

(fset 'yes-or-no-p 'y-or-n-p) ;; enable y/n answers

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Emacs - " (:eval (if (buffer-file-name)
                                                  (abbreviate-file-name (buffer-file-name))
                                                "%b"))))

;; (use-package ov :ensure ov)
(use-package volatile-highlights :ensure volatile-highlights)
;; (use-package browse-kill-ring :ensure browse-kill-ring)

(setq initial-scratch-message "")
(setq-default truncate-lines t)
(tooltip-mode -1)

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
    (set-face-inverse-video-p 'show-paren-match nil)
    (setq show-paren-style 'expression)))

;; http://whattheemacsd.com/init.el-04.html
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

;; (bind-key "C-o"
;;           (lambda () (interactive)
;;             (let ((case-fold-search isearch-case-fold-search))
;;               (occur (if isearch-regexp
;;                          isearch-string
;;                        (regexp-quote isearch-string)))))
;;           isearch-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy :ensure t
  :bind ("M-," . avy-goto-char-2)
  :config
  (progn
    (after 'ace-window
      (after 'hydra
        (defhydra hydra-goto (:exit t)
          "goto"
          ("l" avy-goto-line "line")
          ("i" avy-goto-l "ace line")
          ("c" avy-goto-char-1 "ace char")
          ("w" avy-goto-word-1 "ace word")
          ("d" ace-window  "ace window")))
      (bind-key "C-M-g" 'hydra-goto/body)
      (bind-key "C-'" 'hydra-goto/body))
    
    (setq avy-style 'at-full)
    (setq avy-keys '(?k ?d ?f ?a ?j ?s ?l ?g ?h))))

(use-package ace-window :ensure t)

(use-package swiper :ensure t
  :pin melpa-stable
  :init (progn (bind-key "M-y" 'swiper)
               (bind-key "M-Y" 'swiper))
  :config
  (bind-key "C-o"
            (lambda () (interactive)
              (let ((input (ivy--input)))
                (occur (regexp-quote input))))
            swiper-map))

(use-package ivy
  :pin melpa-stable
  :init (ivy-mode)
  :config
  (progn
    ;; See `ivy-minibuffer-map' for more bindable keys.
    (bind-key "C-m" 'ivy-done ivy-minibuffer-map)
    (bind-key "TAB" 'ivy-alt-done ivy-minibuffer-map)
    (bind-key "M-k" 'ivy-next-line ivy-minibuffer-map)
    (bind-key "M-i" 'ivy-previous-line ivy-minibuffer-map)
    (bind-key "M-d" 'ivy-backward-delete-char ivy-minibuffer-map)
    (bind-key "M-n" 'ivy-beginning-of-buffer ivy-minibuffer-map)
    (bind-key "M-N" 'ivy-end-of-buffer ivy-minibuffer-map)
    (bind-key "M-K" 'ivy-scroll-up-command ivy-minibuffer-map)
    (bind-key "M-I" 'ivy-scroll-down-command ivy-minibuffer-map)))

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; http://whattheemacsd.com/init.el-03.html
(use-package saveplace :ensure t ; remembers your location in a file when saving files
  :defer (progn
           (require 'saveplace)
           (setq-default save-place t))
  :config (setq save-place-file (expand-file-name "saveplace" savefile-dir)))

(use-package savehist                   ; keeps track of some history
  :init (savehist-mode +1)
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
  :defer t
  :config
  (progn
    (recentf-mode +1)
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

;; http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html
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
  :config (if window-system (add-hook 'prog-mode-hook 'hl-line-mode)))

(global-hl-line-mode +1)

(use-package volatile-highlights
  :commands volatile-highlights-mode
  :config (volatile-highlights-mode t)
  :diminish volatile-highlights-mode)

(use-package tramp
  ;; keep in mind known issues with zsh - see emacs wiki
  :config (setq tramp-default-method "ssh"))

(set-default 'imenu-auto-rescan t)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(defun enable-flyspell ()
  (when (executable-find ispell-program-name)
    (flyspell-mode +1)))

(add-hook 'text-mode-hook 'enable-flyspell)

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

;; shorter aliases for ack-and-a-half commands

(use-package ack-and-a-half
  :commands ack
  :config
  (defalias 'ack 'ack-and-a-half)
  (defalias 'ack-same 'ack-and-a-half-same)
  (defalias 'ack-find-file 'ack-and-a-half-find-file)
  (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same))

(use-package dired
  :commands dired
  :config
  (progn
    ;; dired - reuse current buffer by pressing 'a'
    (put 'dired-find-alternate-file 'disabled nil)
    
    ;; always delete and copy recursively
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always)
    ;; if there is a dired buffer displayed in the next window, use its
    ;; current subdir, instead of the current subdir of this dired buffer
    (setq dired-dwim-target t)

    ;; enable some really cool extensions like C-x C-j(dired-jump)
    (use-package dired-x)))

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

(defvar zane/yank-indent-threshold
  10000
  "Threshold beyond which auto-indentation should not happen.")

;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) zane/yank-indent-threshold)
      (indent-region beg end nil)))

(defvar zane/indent-sensitive-modes
  "Modes where we don't want to auto-indent after yanking."
  nil)

(defvar zane/yank-indent-modes
  "Modes where we do want to auto-indent after yanking."
  nil)

(advise-commands "indent" (yank yank-pop) after
                 "If current mode is one of `zane/yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
                 (if (and (not (ad-get-arg 0))
                          (not (member major-mode zane/indent-sensitive-modes))
                          (or (derived-mode-p 'prog-mode)
                              (member major-mode zane/yank-indent-modes)))
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
  :commands re-builder
  :config (setq reb-re-syntax 'string))

(use-package eshell
  :commands eshell
  :config (setq eshell-directory-name
                (expand-file-name "eshell" savefile-dir)))

(use-package semantic
  :config (setq semanticdb-default-save-directory
                (expand-file-name "semanticdb" savefile-dir)))

(use-package winner
  :ensure winner
  :defer t
  :config (winner-mode +1))

(use-package diff-hl :ensure diff-hl
  :commands global-diff-hl-mode
  :defer t
  :config (global-diff-hl-mode +1))

(use-package diff-hl-dired :ensure diff-hl
  :disabled t
  :commands diff-hl-dired-mode
  :init (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package easy-kill
  :ensure easy-kill
  :commands (easy-kill easy-mark)
  
  :init
  (progn
    (bind-key [remap kill-ring-save] 'easy-kill)
    (bind-key [remap mark-sexp] 'easy-mark)))

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
    (unbind-key "M-g" smartparens-mode-map)
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

(use-package clojure-mode
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (fnk 'defun)
    (defnk 'defun)
    (go-try-loop 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (match 'defun)
    (for-all 'defun)
    (project 'defun)))

(use-package clj-refactor :ensure t
  :pin melpa-stable
  
  :config
  (after 'hydra
    (defhydra hydra-clj-add (:exit t)
      "add"
      ("d" cljr-add-declaration "declaration")
      ("i" cljr-add-import-to-ns "import")
      ("l" cljr-add-missing-libspec "missing libspec")
      ("p" cljr-add-project-dependency "project dependency")
      ("r" cljr-add-require-to-ns "require")
      ("s" cljr-add-stubs "stubs")
      ("u" cljr-add-use-to-ns "use"))
    
    (defhydra hydra-clj-remove (:exit t)
      "remove"
      ("l" cljr-remove-let "let")
      ("d" cljr-remove-debug-fns "debug functions")
      ("u" cljr-remove-unused-requires "unused requires")
      ("r" cljr-stop-referring "refer"))

    (defhydra hydra-clj-cycle (:exit t)
      ("c" cljr-cycle-coll "collection")
      ("i" cljr-cycle-if "if")
      ("p" cljr-cycle-privacy "privacy"))

    (defhydra hydra-clj-destructure (:exit t)
      "destructure"
      ("k" cljr-destructure-keys "keys"))

    (defhydra hydra-clj-extract (:exit t)
      ("f" cljr-extract-function "function"))

    (defhydra hydra-clj-let (:exit t)
      "let"
      ("i" cljr-introduce-let "introduce")
      ("e" cljr-expand-let "expand")
      ("m" cljr-move-to-let "move"))

    (defhydra hydra-clj-namespace-add (:exit t)
      "namespace add"
      ("i" cljr-add-import-to-ns "import")
      ("r" cljr-add-require-to-ns "require")
      ("u" cljr-add-use-to-ns "use"))

    (defhydra hydra-clj-namespace (:exit t)
      "namespace"
      ("a" hydra-clj-namespace-add/body "add...")
      ("c" cljr-clean-ns "clean")
      ("s" cljr-sort-ns "sort"))

    (defhydra hydra-clj-thread (:exit t)
      ("t" cljr-thread "thread")
      ("f" cljr-thread-first-all "first all")
      ("l" cljr-thread-last-all "last all"))
    
    (defhydra hydra-clj-refactor (:exit t)
      "refactor"
      ("a" hydra-clj-add/body "add")
      ("r" hydra-clj-remove/body "remove")
      ("c" hydra-clj-cycle/body "cycle")
      ("d" hydra-clj-destructure/body "destructure")
      ("e" hydra-clj-extract/body "extract")
      ("l" hydra-clj-let/body "let")
      ("n" hydra-clj-namespace/body "namespace")
      ("t" hydra-clj-thread/body "thread"))

    (bind-key (kbd "C-M-r") 'hydra-clj-refactor/body clojure-mode-map)

    (defun zane/transpose-sexp-backward ()
      (interactive)

      (save-excursion
        (sp-backward-sexp)
        (sp-transpose-sexp)))
    
    (defhydra hydra-transpose (:exit nil :foreign-keys nil)
      "transpose"
      ("i" zane/transpose-sexp-backward "backward")
      ("j" zane/transpose-sexp-backward "backward")
      ("k" sp-transpose-sexp "forward")
      ("l" sp-transpose-sexp "forward")
      ("q" nil "cancel")
      ("RET" nil "cancel"))

    (bind-key (kbd "C-M-t") 'hydra-transpose/body)))

(use-package scss-mode :ensure t
  :pin melpa-stable)

(use-package magit :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (progn
    (mapc (apply-partially 'add-to-list 'magit-repo-dirs)
          (f-directories user-projects-directory))
    
    (setq magit-status-buffer-switch-function 'switch-to-buffer)
    (setq magit-repo-dirs (list user-projects-directory))
    (setq magit-set-upstream-on-push 'dontask)
    (setq magit-save-some-buffers 'dontask)
    (after 'ido (setq magit-completing-read-function 'ido-completing-read))
    (after 'ivy (setq magit-completing-read-function 'ivy-completing-read))

    ;; http://irreal.org/blog/?p=4279
    (add-to-list 'magit-no-confirm 'stage-all-changes)
    (setq magit-push-always-verify nil)
    (setq magit-last-seen-setup-instructions "2.1.0")

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

(use-package git-timemachine)

(use-package org :ensure t
  :defer t
  :config
  (progn
    (setq org-src-fontify-natively t)
    (setq org-hide-leading-stars t)
    (after 'prelude-mode (add-hook 'org-mode-hook 'prelude-off))
    (unbind-key "M-a" org-mode-map)))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config (when (executable-find "gfm") (setq markdown-command "gfm")))

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
  :pin melpa-stable
  :commands cider-jack-in
  :bind (("S-<return>" . cider-repl-newline-and-indent)
         ("C-c M-r" . cider-refresh))
  
  :config
  (progn
    (unbind-key "C-j" cider-repl-mode-map)
    
    (setq cider-repl-history-file (f-join savefile-dir "cider-repl-history"))
    (setq cider-repl-use-clojure-font-lock t)
    (setq cider-repl-use-pretty-printing nil)
    (setq cider-repl-popup-stacktraces t)
    (setq cider-auto-select-error-buffer nil)
    (setq nrepl-hide-special-buffers nil)
    (setq cider-repl-print-length 100)

    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'subword-mode)))

(use-package zenburn-theme :disabled t)

(use-package color-theme-sanityinc-tomorrow :disabled t
  :if window-system
  :init (load-theme 'sanityinc-tomorrow-night t))

(use-package monokai-theme :ensure t
  :if window-system
  
  :config (setq monokai-high-contrast-mode-line nil)
  
  :init
  (progn (load-theme 'monokai t)
         (set-face-background 'region "#000000")))

(use-package moe-theme :disabled t
  :if window-system
  :init (load-theme 'moe-dark t)
  :config (after 'powerline (powerline-moe-theme)))

(use-package solarized-theme :disabled t
  :if window-system
  :init
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
    (setq solarized-high-contrast-mode-line t)

    (load-theme 'solarized-dark t)))

(use-package cyberpunk-theme :disabled t)

(use-package whitespace
  :if window-system
  
  :init
  (global-whitespace-mode)

  :config
  (progn
    ;; http://xahlee.blogspot.com/2009/08/how-to-use-and-setup-emacss-whitespace.html
    (setq whitespace-trailing-regexp "^.*[^\r\n\t \xA0\x8A0\x920\xE20\xF20]+\\([\t \xA0\x8A0\x920\xE20\xF20]+\\)$")
    (setq whitespace-line-column 100)
    (setq whitespace-action '(auto-cleanup warn-if-read-only))
    (setq whitespace-style '(face tabs empty trailing))

    (defun zane-enable-whitespace ()
      (add-hook 'before-save-hook 'whitespace-cleanup nil t)
      (whitespace-mode +1))
    (add-hook 'text-mode-hook 'zane-enable-whitespace)))

(after 'nrepl-mode 'ok)

(use-package rainbow-delimiters :ensure t
  :commands rainbow-delimiters-mode-enable
  
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))
  
  :config
  (progn
    ;; http://timothypratley.blogspot.com/2015/07/seven-specialty-emacs-settings-with-big.html
    (setq-default frame-background-mode 'dark)
    (setq rainbow-delimiters-max-face-count 1)
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                        :foreground "#666666")
    (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                        :foreground 'unspecified
                        :inherit 'error)))

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

  :init
  (progn
    (add-hook 'prog-mode-hook 'company-mode-on)
    (after 'lisp-mode    (add-hook 'emacs-lisp-mode-hook  'company-mode-on))
    (after 'clojure-mode (add-hook 'clojure-mode-hook     'company-mode-on))
    (after 'cider-repl   (add-hook 'cider-repl-mode-hook  'company-mode-on)))
  
  :config
  (progn
    (bind-key "M-k"     'company-select-next-or-abort     company-active-map)
    (bind-key "M-i"     'company-select-previous-or-abort company-active-map)
    (bind-key "C-f"     'company-search-candidates        company-active-map)
    (bind-key "C-M-f"   'company-filter-candidates        company-active-map)
    (bind-key "C-c C-d" 'company-show-doc-buffer          company-active-map)

    (setq company-idle-delay 0.2)

    (setq company-show-numbers t)))

(use-package web-mode :ensure t
  :mode "\\.html\\'"
  :config (progn (setq web-mode-code-indent-offset 2)
                 (setq web-mode-markup-indent-offset 2)))

(use-package jsx-mode :ensure t
  :mode "\\.jsx\\'"
  :config
  (setq jsx-indent-level 2))

(use-package sql-indent :ensure t
  :mode "\\.sql\\'"
  :config
  (load-library "sql-indent")
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
  :defer t
  :config (progn (require 'fancy-narrow)
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
         ("C-;"         . mc/mark-all-symbols-like-this-in-defun))
  :config (progn (setq mc/list-file (f-expand ".mc-lists.el" savefile-dir))
                 (bind-key "C-;" 'mc/mark-all-symbols-like-this-in-defun prog-mode-map)))

(use-package expand-region :ensure t
  :bind (("M->" . er/expand-region)
         ("M-<" . er/contract-region))
  :init
  (after 'hydra
    (setq expand-region-fast-keys-enabled nil)
    (defhydra hydra-expand-region ()
      "expand region"
      ("." er/expand-region "expand")
      ("\," er/contract-region "contract"))
    (bind-key "M->" 'hydra-expand-region/er/expand-region)
    (bind-key "M-<" 'hydra-expand-region/er/contract-region))
  :config
  (setq expand-region-contract-fast-key "<"))

(use-package key-chord
  :disabled t)

(use-package projectile
  :ensure t
  ;; :pin melpa-stable
  :commands projectile-global-mode

  :init
  (setq projectile-known-projects-file (f-join savefile-dir "projectile-bookmarks.eld")
        projectile-cache-file          (f-join savefile-dir "projectile.cache"))
  (after 'ivy (setq projectile-completion-system 'ivy))
  (projectile-global-mode t))

(use-package perspective :ensure t
  :init
  (persp-mode))

(use-package persp-projectile :ensure t
  :config
  (require 'persp-projectile)
  
  :init
  (bind-key (kbd "C-c p p") 'projectile-persp-switch-project projectile-mode-map))

(use-package ido :disabled t
  :init (ido-mode +1)
  :config
  (progn
    (bind-key "C-o" 'ido-find-file)
    (bind-key "C-x b" 'ido-switch-buffer)
    
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

(use-package zoom-frm ; vendored in the `lisp' subdirectory
  :init (use-package frame-cmds)
  :if window-system
  :bind (("M-+" . zoom-frm-in)
         ("M--" . zoom-frm-out)
         ("M-0" . zoom-frm-unzoom)))

(use-package hydra :ensure t)

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
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("b06aaf5cefc4043ba018ca497a9414141341cb5a2152db84a9a80020d35644d1" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "d7088a7105aa09cc68e3d058f89917e07e0505e0f4ab522a6045ec8092d67c44" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default)))
 '(fci-rule-color "#49483E")
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (("#49483E" . 0)
    ("#67930F" . 20)
    ("#349B8D" . 30)
    ("#21889B" . 50)
    ("#968B26" . 60)
    ("#A45E0A" . 70)
    ("#A41F99" . 85)
    ("#49483E" . 100)))
 '(magit-diff-use-overlays nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
