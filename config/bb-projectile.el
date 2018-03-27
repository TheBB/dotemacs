(bb-package projectile
  :init
  (bb-leader "ga" 'projectile-find-other-file)
  (projectile-mode)
  (diminish 'projectile-mode))
