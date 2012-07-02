(defun enable-speck-mode ()
  "Turn on speck spellchecking."
  (interactive)
  (speck-mode t))

(add-hook 'text-mode-hook 'enable-speck-mode)
