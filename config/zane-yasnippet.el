;; svn checkout http://yasnippet.googlecode.com/svn/trunk/ ~/Projects/yasnippet.googlecode
(add-to-list 'load-path (concat zane-projects-dir "yasnippet.googlecode"))
(require 'yasnippet)
(eval-after-load "yasnippet"
  '(progn
     (yas/initialize)
     (yas/load-directory (concat zane-projects-dir "yasnippet.googlecode/snippets"))))
