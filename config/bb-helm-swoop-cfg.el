(defun bb-helm-swoop ()
  (interactive)
  (let ((helm-echo-input-in-header-line t))
    (call-interactively 'helm-swoop)))
