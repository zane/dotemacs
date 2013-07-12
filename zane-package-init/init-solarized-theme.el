(after "solarized-theme-autoloads"
  (when window-system
    (require 'solarized)
    (load-theme 'solarized-dark t)
    (set-face-background 'cursor "#d33682")))
