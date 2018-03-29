(defun bb-elisp ()
  (push '("Package" "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2) imenu-generic-expression))

(provide 'bb-hooks)
