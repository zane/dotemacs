(eval-after-load 'package
  '(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/")))

(enable-theme 'tango-dark)

(setq mac-command-modifier 'meta)

(global-set-key (kbd "C-x y") 'bury-buffer)
(global-set-key "\C-o" 'other-window)

(global-set-key "\M-r" 'isearch-backward)
(global-set-key "\M-s" 'isearch-forward)

;; Key for fullscreen from custom build of Emacs.app:
;; http://www.stratospark.com/blog/2010/fullscreen_emacs_on_osx.html
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)

(setq server-use-tcp t)
