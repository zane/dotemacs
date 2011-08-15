(add-to-list 'load-path (concat zane-emacs-root "../jade-mode.github.brianc"))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(autoload 'sws-mode "sws-mode")
(autoload 'jade-mode "jade-mode")
