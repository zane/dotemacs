;; http://xahlee.blogspot.com/2009/08/how-to-use-and-setup-emacss-whitespace.html
(setq whitespace-trailing-regexp
      "\\S-.*?\\(\\(\t\\| \\|\xA0\\|\x8A0\\|\x920\\|\xE20\\|\xF20\\)+\\)$")
(setq whitespace-style '(face tabs trailing empty))
(setq whitespace-action '(auto-cleanup warn-if-read-only))
(global-whitespace-mode 1)
