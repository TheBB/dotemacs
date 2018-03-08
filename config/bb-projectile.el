(bb-package projectile
  :init
  (setq projectile-known-projects-file
        (bb-dir ".cache/projectile-bookmarks.eld"))
  (projectile-mode)
  (diminish 'projectile-mode))
