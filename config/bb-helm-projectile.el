(bb-package helm-projectile
  :init
  (setq projectile-switch-project-action 'helm-projectile)
  (bb-autoload "helm-projectile"
    helm-projectile
    helm-projectile-find-dir
    helm-projectile-find-file
    helm-projectile-switch-project
    helm-projectile-switch-to-buffer)
  (with-eval-after-load 'helm-projectile
    (define-key helm-projectile-find-file-map (kbd "<right>") 'helm-maybe-exit-minibuffer))
  :post-init general
  (bb-leader "pb" 'helm-projectile-switch-to-buffer
             "pd" 'helm-projectile-find-dir
             "pf" 'helm-projectile-find-file
             "ph" 'helm-projectile
             "pp" 'helm-projectile-switch-project))
