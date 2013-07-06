(after 'org
  (setq org-default-notes-file (concat org-directory "/inbox.org")))

(define-key global-map (kbd "<f7>") 'org-capture)
