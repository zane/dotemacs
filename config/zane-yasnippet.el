;; svn checkout http://yasnippet.googlecode.com/svn/trunk/ ~/Projects yasnippet.googlecode
(add-to-list 'load-path (concat zane-emacs-root "../yasnippet.googlecode"))
(require 'yasnippet)
(eval-after-load "yasnippet"
  '(progn
     (yas/initialize)
     (yas/load-directory (concat zane-emacs-root "../yasnippet.googlecode/snippets"))))
