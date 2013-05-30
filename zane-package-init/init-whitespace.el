;; http://xahlee.blogspot.com/2009/08/how-to-use-and-setup-emacss-whitespace.html
(after 'whitespace
  (setq whitespace-style '(face tabs empty))
  (setq whitespace-action '(auto-cleanup warn-if-read-only))
  (global-whitespace-mode 1))
