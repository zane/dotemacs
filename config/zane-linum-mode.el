
;; (global-linum-mode 1)

;; FIXME: This is broken... Figure out how to set this programmatically.
;; (fringe-mode 'no-fringes)

;; Add a space between line numbers and file text.
(setq linum-format
      (lambda (line)
        (propertize (format
                     (let ((w (length (number-to-string
                                       (count-lines (point-min) (point-max))))))
                       (concat "%" (number-to-string w) "d "))
                     line)
                    'face 'linum)))
