(require 'yasnippet)

(after 'yasnippet
  (after 'auto-complete
    (require 'auto-complete-config)
    (add-to-list 'ac-sources 'ac-source-yasnippet))

  ;; (setq yas-indent-line 'fixed)
  (setq yas/root-directory (expand-file-name (concat user-emacs-directory "zane-yasnippets")))
  (yas/load-directory yas/root-directory)

  (yas/global-mode 1))

