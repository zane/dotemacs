(require 'image-dired)


;; Refresh the dired buffer whenever it regains focus
(setq-default dired-auto-revert-buffer t)
(setq dired-use-ls-dired nil)

(setq dired-listing-switches "-alh")
