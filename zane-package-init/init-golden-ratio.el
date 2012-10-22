(require 'golden-ratio)

;; golden-ratio seems to have issues balancing the widths of
;; non-active horizontal siblings. The following advice forces this
;; behavior.

(defadvice golden-ratio
  (after horizontally-balance-after-golden activate)
  (zane/balance-sibling-widths))

;; golden-ratio and ediff do not play well together. Only use it when
;; we're explicitly changing between windows.

(defadvice windmove-left
  (after windmove-left-golden-ratio)
  (golden-ratio))

(defadvice windmove-right
  (after windmove-right-golden-ratio)
  (golden-ratio))

(defadvice windmove-up
  (after windmove-up-golden-ratio)
  (golden-ratio))

(defadvice windmove-down
  (after windmove-down-golden-ratio)
  (golden-ratio))

(defun zane/golden-ratio-enable ()
  (interactive)
  (ad-enable-advice windmove-left  'after 'windmove-left-golden-ratio)
  (ad-enable-advice windmove-right 'after 'windmove-right-golden-ratio)
  (ad-enable-advice windmove-up    'after 'windmove-up-golden-ratio)
  (ad-enable-advice windmove-down  'after 'windmove-down-golden-ratio))

(defun zane/golden-ratio-disable ()
  (interactive)
  (ad-disable-advice 'windmove-left-golden-ratio)
  (ad-disable-advice 'windmove-right-golden-ratio)
  (ad-disable-advice 'windmove-up-golden-ratio)
  (ad-disable-advice 'windmove-down-golden-ratio))
