(after 'js
  ;;(define-key js-mode-map "{" 'paredit-open-curly)
  ;;(define-key js-mode-map "}" 'paredit-close-curly-and-newline)
  (setq js-indent-level 2)
  (define-key js-mode-map (kbd ",") 'self-insert-command)
  (define-key js-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (font-lock-add-keywords 'js-mode `(("\\(function *\\)("
                                      (0 (progn (compose-region (match-beginning 1)
                                                                (match-end 1)
                                                                "ƒ")
                                                nil))))))

(add-hook 'java-mode-hook
          (lambda ()
            (subword-mode t)
            (setq tab-width 4)))

(after 'python
  (setq python-python-command "ipython")
  ;; We never want to edit Python bytecode
  (add-to-list 'completion-ignored-extensions ".rbc"))

(after 'coffee-mode
  (defun z:set-tab-with-to-2 ()
    (set (make-local-variable 'tab-width) 2))
  (add-hook 'coffee-mode-hook 'z:set-tab-with-to-2))

(after 'ruby-mode
  (after 'ergoemacs-mode
    (defun z:reset-ret ()
      (ergoemacs-local-set-key (kbd "RET") 'reindent-then-newline-and-indent))
    (add-hook 'ruby-mode-hook 'z:reset-ret)))

(after "smartparens-autoloads"
  (setq sp-base-key-bindings 'paredit)
  
  (defun turn-on-smartparens-mode ()
    "Turn on `smartparens-mode'."
    (smartparens-mode +1))

  (dolist (mode-hook '(ruby-mode-hook
                       python-mode-hook
                       js-mode-hook))
    (add-hook mode-hook 'turn-on-smartparens-mode)))

(after "flycheck-autoloads"
  ;; Use flycheck for all modes that aren't emacs-lisp-mode
  (defun z:maybe-turn-on-flycheck-mode ()
    (when (not (equal 'emacs-lisp-mode major-mode))
      (flycheck-mode)))
  (add-hook 'find-file-hook 'z:maybe-turn-on-flycheck-mode)
  
  (after 'flycheck
    (set-face-attribute 'flycheck-error nil :underline "red")
    (set-face-attribute 'flycheck-warning nil :underline "yellow")
    (setq flycheck-mode-line-lighter " Κ")))

(dolist (mode '(ruby-mode-hook
                js-mode-hook))
  (add-hook mode 'subword-mode))

(autoload 'enable-paredit-mode "paredit")

(after "paredit-autoloads"
  (after 'clojure-mode-autoloads (add-hook 'clojure-mode-hook 'enable-paredit-mode))
  (after 'slime-autoloads (add-hook 'slime-repl-mode-hook 'enable-paredit-mode))

  (dolist (hook '(emacs-lisp-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'enable-paredit-mode))

  (after 'paredit
    ;; making paredit work with delete-selection-mode
    ;; https://github.com/malkomalko/.emacs.d/blob/master/modules/setup-paredit.el
    (put 'paredit-forward-delete 'delete-selection 'supersede)
    (put 'paredit-backward-delete 'delete-selection 'supersede)
    (put 'paredit-open-round 'delete-selection t)
    (put 'paredit-open-square 'delete-selection t)
    (put 'paredit-doublequote 'delete-selection t)
    (put 'paredit-newline 'delete-selection t)

    (defadvice yank
      (after yank-indent-pp-sexp activate)
      (save-excursion
        (if paredit-mode
            (condition-case ex
                (progn
                  (paredit-backward-up)
                  (indent-pp-sexp))
              ('error nil)))))

    (after 'ergoemacs-mode
      (dolist (key (list (kbd "M-;")    ; searching
                         (kbd "C-d")    ; was paredit-forward-delete
                         (kbd "C-d")    ; was paredit-backward-delete
                         (kbd "M-d")    ; was paredit-backward-kill-word
                         (kbd "M-d")    ; was paredit-forward-kill-word
                         (kbd "C-k")    ; was paredit-kill
                         (kbd "M-J")))  ; moving to the beginning of the file
        (define-key paredit-mode-map key nil))

      (add-hook 'paredit-mode-hook
                (lambda ()
                  (ergoemacs-local-set-key (kbd "M-f") 'paredit-forward-delete)
                  (ergoemacs-local-set-key (kbd "M-d") 'paredit-backward-delete)
                  (ergoemacs-local-set-key (kbd "M-e") 'paredit-backward-kill-word)
                  (ergoemacs-local-set-key (kbd "M-r") 'paredit-forward-kill-word)
                  (ergoemacs-local-set-key (kbd "M-g") 'paredit-kill)
                  )))

    (after 'windmove
      ;; windmove-left
      (define-key paredit-mode-map (kbd "C-j") nil)
      ;; windmove-down
      (define-key paredit-mode-map (kbd "C-k") nil))))

(after "rainbow-delimiters-autoloads"
  (defun z:turn-on-rainbow-delimiters-mode ()
    (interactive)
    (rainbow-delimiters-mode 1))

  (setq-default frame-background-mode 'dark)
  (let ((hooks '(emacs-lisp-mode-hook
                 clojure-mode-hook
                 javascript-mode-hook
                 lisp-mode-hook
                 python-mode-hook)))
    (dolist (hook hooks)
      (add-hook hook 'z:turn-on-rainbow-delimiters-mode))))

(provide 'zane-coding)
