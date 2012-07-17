;;;; egroemacs keybindings

;; FIX: for some reason the library isn't loading from the package?
;;      Have to put full path to the load file, which is brittle
(when (package-installed-p 'ergoemacs-keybindings)
  (load-file "~/.emacs.d/elpa/ergoemacs-keybindings-20120710/ergoemacs-mode.el")
  (ergoemacs-mode 1))

(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us")
(defun z:ergoemacs-ido-minibuffer-setup-hook ()
  (ergoemacs-ido-minibuffer-setup-hook)
  ;; Swap the prev/next and prev/next-dir keys
  ;; since we're displaying ido candidates
  ;; vertically instead of horizontally
  (define-key ergoemacs-ido-keymap ergoemacs-forward-char-key 'ido-next-match-dir) ; was 'ido-prev-match-dir
  (define-key ergoemacs-ido-keymap ergoemacs-backward-char-key 'ido-prev-match-dir) ; was 'ido-prev-match
  (define-key ergoemacs-ido-keymap ergoemacs-previous-line-key 'ido-prev-match) ; was 'ido-next-match-dir
  (define-key ergoemacs-ido-keymap ergoemacs-next-line-key 'ido-next-match) ; was 'ido-prev-match-dir
  )

(remove-hook 'ido-minibuffer-setup-hook 'ergoemacs-ido-minibuffer-setup-hook)
(add-hook 'ido-minibuffer-setup-hook 'z:ergoemacs-ido-minibuffer-setup-hook)
(ergoemacs-mode 1)
