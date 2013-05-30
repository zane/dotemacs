(after 'diminish-autoloads
  (defmacro rename-modeline (package-name mode new-name)
    `(eval-after-load ,package-name
       '(defadvice ,mode (after rename-modeline activate)
          (setq mode-name ,new-name))))

  ;; http://whattheemacsd.com//appearance.el-01.html
  (rename-modeline 'lisp-mode emacs-lisp-mode "ELisp")
  (rename-modeline 'js js-mode "JS")

  (after 'paredit         (diminish 'paredit-mode           " Φ"))
  (after 'yasnippet       (diminish 'yas-minor-mode         " Υ"))
  (after 'ergoemacs-mode  (diminish 'ergoemacs-mode         " Ε"))
  (after 'auto-complete   (diminish 'auto-complete-mode     " Α"))
  (after 'whitespace      (diminish 'global-whitespace-mode " Θ"))
  (after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode   " sn"))
  (after 'eldoc           (diminish 'eldoc-mode             " ed"))
  (after 'simple          (diminish 'auto-fill-function     " af")))
