(bb-package projectile
  :init
  (setq projectile-known-projects-file (bb-dir ".cache/projectile-bookmarks.eld")
        projectile-cache-file (bb-dir ".cache/projectile.cache"))
  (bb-leader "ga" 'projectile-find-other-file)
  (projectile-mode)
  (diminish 'projectile-mode))
