(after 'diminish-autoloads
  (defmacro rename-modeline (package-name mode new-name)
    `(eval-after-load ,package-name
       '(defadvice ,mode (after rename-modeline activate)
          (setq mode-name ,new-name))))

  ;; http://whattheemacsd.com//appearance.el-01.html
  (rename-modeline 'lisp-mode emacs-lisp-mode "EL")
  (rename-modeline 'js js-mode "JS")
  (rename-modeline 'lisp-mode lisp-interaction-mode "LI")

  (after 'eldoc           (diminish 'eldoc-mode             " ed"))
  (after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode   " sn"))
  (after 'paredit         (diminish 'paredit-mode           " pe"))
  (after 'simple          (diminish 'auto-fill-function     " af"))

  (after 'auto-complete       (diminish 'auto-complete-mode))
  (after 'ergoemacs-mode      (diminish 'ergoemacs-mode))
  (after 'undo-tree           (diminish 'undo-tree-mode))
  (after 'volatile-highlights (diminish 'volatile-highlights-mode))
  (after 'whitespace          (diminish 'global-whitespace-mode))
  (after 'yasnippet           (diminish 'yas-minor-mode)))
