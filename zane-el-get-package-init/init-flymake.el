(defun flymake-xml-init ()
  (list "xmllint"
        (list "--valid"
              (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))

(defun flymake-html-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "tidy" (list local-file))))

;; (add-to-list 'flymake-allowed-file-name-masks
;;              '("\\.html$\\|\\.ctp" flymake-html-init))

;; (add-to-list 'flymake-err-line-patterns
;;              '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
;;                nil 1 2 4))

(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "/Users/zshelby/.emacs.d/bin/pycheckers.sh" (list local-file))))

(defun flymake-ruby-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(when (load "flymake" t)
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)
               '("\\.rb\\'" flymake-ruby-init))

  (add-hook 'find-file-hook 'flymake-find-file-hook))

(when (load "flymake" t)
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.rb\\'" flymake-ruby-init)))

(setq flymake-gui-warnings-enabled t)

(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))
