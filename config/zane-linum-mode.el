(global-linum-mode 1)
(fringe-mode -1)
(setq linum-format
      (lambda (line)
        (propertize (format
                     (let ((w (length (number-to-string
                                       (count-lines (point-min) (point-max))))))
                       (concat "%" (number-to-string w) "d "))
                     line)
                    'face 'linum)))

(add-to-list 'load-path (concat zane-emacs-root "lib"))
(require 'linum-off)
(add-to-list 'linum-disabled-modes-list 'fundamental-mode)
