(defun z:mac-p ()
  "Truthy if the host OS is a Mac."
  (string-match "apple-darwin" system-configuration))

(defun z:deduplicate-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (z:uniquify-all-lines-region start end))

(defun z:deduplicate-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (z:uniquify-all-lines-buffer))

(defun z:uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun z:uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (z:uniquify-all-lines-region (point-min) (point-max)))

(defun z:set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun z:switch-to-next-frame ()
  "Select the next frame on current display, and raise it."
  (interactive)
  (other-frame 1))

(defun z:switch-to-previous-frame ()
  "Select the previous frame on current display, and raise it."
  (interactive)
  (other-frame -1))

;; http://emacswiki.org/emacs/TransposeWindows
(defun z:rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let ((i 1)
          (num-windows (count-windows)))
      (while  (< i num-windows)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i num-windows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(defun z:smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive) ; Use (interactive "^") in Emacs 23 to make shift-select work
  (let ((oldpos (point)))
    (beginning-of-line-text)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun z:vagrant-shell ()
  (interactive)
  (let ((default-directory "/vagrant:/home/vagrant/projects"))
    (shell "*vagrant-root*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Case Toggling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zane/split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([A-Z]\\)\\([A-Z]\\)" "\\1 \\2"
       (replace-regexp-in-string "\\([a-zA-Z]\\)\\([A-Z]\\)" "\\1 \\2" s))))
   "[^A-Za-z0-9]+"))

(defun zane/camel-case (s) (mapconcat 'capitalize (zane/split-name s) ""))
(defun zane/underscore-case (s) (mapconcat 'downcase (zane/split-name s) "_"))
(defun zane/dash-case (s) (mapconcat 'downcase (zane/split-name s) "-"))
(defun zane/double-colon-case (s) (mapconcat 'capitalize (zane/split-name s) "::"))

(defun zane/double-colon-case-p (s) (string-match-p "\:" s))
(defun zane/dash-case-p (s) (string-match-p "-" s))
(defun zane/underscore-case-p (s) (string-match-p "_" s))

(defun zane/toggle-identifier-case (s)
  (cond ((zane/double-colon-case-p s) (zane/camel-case s))
        ((zane/dash-case-p s) (zane/double-colon-case s))
        ((zane/underscore-case-p s) (zane/dash-case s))
        (t (zane/underscore-case s))))

(defun zane/toggle-identifier-case-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (zane/toggle-identifier-case txt)) )
    (if cml (progn (delete-region beg end) (insert cml))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'zane-funcs)
