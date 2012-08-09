;; http://sakito.jp/emacs/emacsshell.html

(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(setq system-uses-terminfo nil)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Execute the following shell command, correcting as appropriate:
;; $ tic -o ~/.terminfo /usr/local/Cellar/emacs/HEAD/share/emacs/24.0.50/etc/e/eterm-color.ti

;; http://www.faqs.org/faqs/unix-faq/shell/zsh/#ixzz0TyTJsHMq
;; http://www.masteringemacs.org/articles/2010/11/01/running-shells-in-emacs-overview/

;; http://tapoueh.org/blog/2011/07/29-emacs-ansi-colors.html
(require 'ansi-color)
(setq ansi-color-names-vector
      (vector (frame-parameter nil 'background-color)
               "#586e75" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#93a1a1")
      ansi-term-color-vector ansi-color-names-vector
      ansi-color-map (ansi-color-make-color-map))
