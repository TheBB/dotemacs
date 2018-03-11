(defun bb-helm-ag-project ()
  (interactive)
  (helm-do-ag (projectile-project-root)))
