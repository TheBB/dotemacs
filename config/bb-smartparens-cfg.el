(defun bb-sp-pair-newline-and-indent (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode))
  (indent-according-to-mode))

(defmacro bb-apply-newline-indent (modes &rest pairs)
  `(progn
     ,@(cl-loop for pair in pairs
                collect `(sp-local-pair ',modes ,pair nil :post-handlers
                                        '(:add (bb-sp-pair-newline-and-indent "RET"))))))
