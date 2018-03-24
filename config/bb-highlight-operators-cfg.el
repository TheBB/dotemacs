(defun bb-maybe-highlight-operators ()
  (unless (derived-mode-p 'lisp-mode 'scheme-mode 'emacs-lisp-mode 'python-mode)
    (highlight-operators-mode)))
