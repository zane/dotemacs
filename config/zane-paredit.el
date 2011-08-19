(defvar paredit-no-space-list '(python-mode)
  "The list of major modes for which paredit should refrain appending a space
   when inserting a matching delimiter.")

(defadvice paredit-space-for-delimiter-p (around no-space-in-modes)
  "Do not put spaces after inserted delimiters in the modes
  listed in paredit-no-space-list."
  (if (member major-mode paredit-no-space-list)
      nil
    ad-do-it))
