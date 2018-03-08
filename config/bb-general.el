(bb-package general
  :boot
  (setq general-override-states '(normal visual motion))
  (require 'general)
  (general-override-mode))
