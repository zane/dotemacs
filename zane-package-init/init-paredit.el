(defvar paredit-no-space-list '(python-mode
                                html-mode)
  "The list of major modes for which paredit should refrain appending a space
                            when inserting a matching delimiter.")

(add-to-list 'paredit-space-for-delimiter-predicates
             (lambda (endp delimiter)
               (not (member major-mode paredit-no-space-list))))

;; C-j conflicts with windmove-left
(define-key paredit-mode-map (kbd "C-j") nil)
;; C-k conflicts with windmove-down
(define-key paredit-mode-map (kbd "C-k") nil)
;; C-; conflicts with searching
(define-key paredit-mode-map (kbd "M-;") nil)
;; M-J conflicts with moving to the beginning of the file
(define-key paredit-mode-map (kbd "M-J") nil)

(define-key paredit-mode-map (kbd "C-d") nil) ; was paredit-forward-delete
(define-key paredit-mode-map (kbd "M-f") 'paredit-forward-delete)

(define-key paredit-mode-map (kbd "C-d") nil) ; was paredit-backward-delete
(define-key paredit-mode-map (kbd "M-d") 'paredit-backward-delete)

(define-key paredit-mode-map (kbd "M-d") nil) ; was paredit-backward-kill-word
(define-key paredit-mode-map (kbd "M-e") 'paredit-backward-kill-word)

(define-key paredit-mode-map (kbd "M-d") nil) ; was paredit-forward-kill-word
(define-key paredit-mode-map (kbd "M-r") 'paredit-forward-kill-word)

(define-key paredit-mode-map (kbd "C-k") nil) ; was paredit-kill
(define-key paredit-mode-map (kbd "M-g") 'paredit-kill)
