;; Get el-get and install it if we don't have it already.
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

;; local sources
(setq el-get-sources
      '(
	clojure-mode
	color-theme
	color-theme-solarized
	color-theme-zenburn
	full-ack
	magit
	markdown-mode
	paredit
	rinari
	slime
	))

(require 'el-get)

(el-get 'sync el-get-sources)
(el-get 'wait)

(provide 'zane-el-get)
