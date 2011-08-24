(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

;; Get el-get and install it if we don't have it already.
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(el-get 'sync)

(provide 'zane-el-get)
