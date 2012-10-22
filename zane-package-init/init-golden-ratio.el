(require 'golden-ratio)

(golden-ratio-enable)

;; golden-ratio seems to have issues balancing the widths of
;; non-active horizontal siblings. The following advice forces this
;; behavior.

(defadvice golden-ratio
  (after horizontally-balance-after-golden)
  (zane/balance-sibling-widths))

;; golden-ratio and ediff do not play well together. The following
;; functions and advice temporarily disable golden-ratio while ediff
;; is active.

(defun ediffingp ()
  (let ((ediffing nil))
    (walk-window-tree
     (lambda (window)
       (when (equal "*Ediff Control Panel*"
                    (buffer-name (window-buffer window)))
         (setq ediffing t))))
    ediffing))

(defvar setting-up-ediff nil
  "True when magit is setting up ediff.")

(defadvice magit-ediff
  (around set-ediff-setup)
  (setq setting-up-ediff t)
  ad-do-it
  (setq setting-up-ediff nil))

(ad-activate 'magit-ediff)

(defadvice golden-ratio
  (around disable-when-ediff)
  (unless (or (ediffingp) setting-up-ediff)
    (progn (message "Doing it!")
      ad-do-it)))

(ad-activate 'golden-ratio)
