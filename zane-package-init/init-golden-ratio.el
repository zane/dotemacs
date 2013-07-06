;; golden-ratio seems to have issues balancing the widths of
;; non-active horizontal siblings. The following advice forces this
;; behavior.

(after 'golden-ratio-autoloads 
  (defun zane/golden-ratio-enable ()
    (interactive)
    (ad-enable-advice 'windmove-left  'after 'windmove-left-golden-ratio)
    (ad-activate 'windmove-left)
    (ad-enable-advice 'windmove-right 'after 'windmove-right-golden-ratio)
    (ad-activate 'windmove-right)
    (ad-enable-advice 'windmove-up    'after 'windmove-up-golden-ratio)
    (ad-activate 'windmove-up)
    (ad-enable-advice 'windmove-down  'after 'windmove-down-golden-ratio)
    (ad-activate 'windmove-down))

  (defun zane/golden-ratio-disable ()
    (interactive)
    (ad-disable-advice 'windmove-left  'after 'windmove-left-golden-ratio)
    (ad-activate 'windmove-left)
    (ad-disable-advice 'windmove-right 'after 'windmove-right-golden-ratio)
    (ad-activate 'windmove-right)
    (ad-disable-advice 'windmove-up    'after 'windmove-up-golden-ratio)
    (ad-activate 'windmove-up)
    (ad-disable-advice 'windmove-down  'after 'windmove-down-golden-ratio)
    (ad-activate 'windmove-down)))

(after 'golden-ratio
  (defadvice golden-ratio
    (after horizontally-balance-after-golden)
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
    (golden-ratio)))
