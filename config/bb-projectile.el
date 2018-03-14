(bb-package projectile
  :init
  (setq projectile-known-projects-file
        (bb-dir ".cache/projectile-bookmarks.eld"))
  (bb-leader "ga" 'projectile-find-other-file)
  (projectile-mode)
  (diminish 'projectile-mode))
