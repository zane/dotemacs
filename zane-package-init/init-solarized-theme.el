(after "solarized-theme-autoloads"
  (when window-system
    (load-theme 'solarized-dark t)
    (set-face-background 'cursor "#d33682")))
