(require 'flymake-jshint)

(eval-after-load "flymake-jshint"
  '(setq jshint-configuration-path (expand-file-name (concat z:package-init-dir "jshint/jshint-config.json"))))

(add-hook 'js-mode-hook 'flymake-jshint-load)