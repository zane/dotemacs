(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us")
(defun zane-ergoemacs-ido-minibuffer-setup-hook ()
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
(add-hook 'ido-minibuffer-setup-hook 'zane-ergoemacs-ido-minibuffer-setup-hook)
(ergoemacs-mode 1)
(message "***INITTING ERGOEMACS**")
