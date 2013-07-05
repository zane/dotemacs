;;;; egroemacs keybindings

;; FIX: for some reason the library isn't loading from the package?
(when (package-installed-p 'ergoemacs-keybindings)
  (require 'ergoemacs-mode))

(after 'ergoemacs-mode
  (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us")
  (defun z:ergoemacs-ido-minibuffer-setup-hook ()
    (ergoemacs-ido-minibuffer-setup-hook)
    ;; Swap the prev/next and prev/next-dir keys
    ;; since we're displaying ido candidates
    ;; vertically instead of horizontally

    ;; was 'ido-prev-match-dir
    (define-key ergoemacs-ido-keymap ergoemacs-forward-char-key 'ido-next-match-dir)
    ;; was 'ido-prev-match
    (define-key ergoemacs-ido-keymap ergoemacs-backward-char-key 'ido-prev-match-dir)
    ;; was 'ido-next-match-dir
    (define-key ergoemacs-ido-keymap ergoemacs-previous-line-key 'ido-prev-match)
    ;; was 'ido-prev-match-dir
    (define-key ergoemacs-ido-keymap ergoemacs-next-line-key 'ido-next-match))
  
  (ergoemacs-global-set-key (kbd "C-/") 'zane/toggle-identifier-case-word-at-point)
  (ergoemacs-global-set-key [remap move-beginning-of-line]
                            'zane/smarter-move-beginning-of-line)

  (remove-hook 'ido-minibuffer-setup-hook 'ergoemacs-ido-minibuffer-setup-hook)
  (add-hook 'ido-minibuffer-setup-hook 'z:ergoemacs-ido-minibuffer-setup-hook)

  (ergoemacs-mode 1)
  
  (cua-mode -1))

