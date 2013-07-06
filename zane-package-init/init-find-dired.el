;; http://www.masteringemacs.org/articles/2011/03/25/working-multiple-files-dired/
(after 'find-dired
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))
