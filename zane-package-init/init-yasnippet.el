(require 'yasnippet)

(defun use-ac-yasnippet ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(eval-after-load 'auto-complete
  '(progn
     (require 'auto-complete-config)
     (add-to-list 'prog-mode-hook 'use-ac-yasnippet)))

(setq yas-indent-line 'fixed)
(setq yas/root-directory (expand-file-name (concat user-emacs-directory "zane-yasnippets")))
(yas/load-directory yas/root-directory)

(yas/global-mode 1)
