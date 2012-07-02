(defun z:mac-p ()
  "Truthy if the host OS is a Mac."
  (string-match "apple-darwin" system-configuration))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun switch-to-next-frame ()
  "Select the next frame on current display, and raise it."
  (interactive)
  (other-frame 1))

(defun switch-to-previous-frame ()
  "Select the previous frame on current display, and raise it."
  (interactive)
  (other-frame -1))

(provide 'zane-funcs)
