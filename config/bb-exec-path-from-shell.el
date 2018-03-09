(bb-package exec-path-from-shell
  :init
  ;; TODO: Figure out what is causing this
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))
