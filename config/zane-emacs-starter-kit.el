;;; zane-emacs-starter-kit.el
;;;
;;; Personalization of the defaults provided by the emacs-starter-kit.
;;; See https://github.com/technomancy/emacs-starter-kit for more
;;; information on the Emacs Starter Kit.

(add-hook 'text-mode-hook (lambda () (speck-mode t)))
(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-flyspell)

(add-hook 'esk-coding-hook (lambda () (setq truncate-lines t)))
(remove-hook 'esk-coding-hook 'esk-turn-on-hl-line-mode)
