(require 'image-dired)

(after 'dired
  ;; Refresh the dired buffer whenever it regains focus
  (setq-default dired-auto-revert-buffer t)

  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  
  (setq dired-use-ls-dired nil)
  (setq dired-listing-switches "-alh"))
