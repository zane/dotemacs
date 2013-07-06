(require 'ergoemacs-mode)

(after 'ergoemacs-mode
  (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us")

  (ergoemacs-defvariant zane
    "Zane Shelby variant -- ALL key except <apps> and <menu> keys."
    nil
    (ergoemacs-replace-key 'ergoemacs-smex-if-exists "M-a" "M-a")
    (ergoemacs-replace-key  'ergoemacs-cut-line-or-region "M-x" "✂ region")
    (ergoemacs-minor-key 'ido-minibuffer-setup-hook '(forward-char ido-next-match-dir minor-mode-overriding-map-alist))
    (ergoemacs-minor-key 'ido-minibuffer-setup-hook '(backward-char ido-prev-match-dir minor-mode-overriding-map-alist))
    (ergoemacs-minor-key 'ido-minibuffer-setup-hook '(previous-line ido-prev-match minor-mode-overriding-map-alist))
    (ergoemacs-minor-key 'ido-minibuffer-setup-hook '(next-line ido-next-match minor-mode-overriding-map-alist))
    (setq ergoemacs-variable-layout-tmp
          (remove-if (lambda (x) (or (string-match "<apps>" (car x))
                                (string-match "<menu>" (car x))))
                     ergoemacs-variable-layout)))
  (setq ergoemacs-variant "zane")
  (ergoemacs-mode 1)
  
  (global-set-key (kbd "C-/") 'zane/toggle-identifier-case-word-at-point)
  (global-set-key [remap move-beginning-of-line]
                  'zane/smarter-move-beginning-of-line)
  
  (cua-mode -1))
