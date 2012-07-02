(eval-after-load "helm-mode"
  '(progn
     (define-key helm-map (kbd "M-k") 'helm-next-line)
     (define-key helm-map (kbd "M-i") 'helm-previous-line)))
