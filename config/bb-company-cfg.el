(defmacro bb-company (mode &rest backends)
  (let ((funcname (intern (format "bb-company-%s" mode)))
        (hookname (intern (format "%s-hook" mode))))
    `(progn
       (defun ,funcname ()
         (company-mode)
         (setq-local company-backends ',backends))
       (add-hook ',hookname ',funcname))))

(defun bb-company-lsp ()
  (company-mode)
  (setq-local company-backends '(company-lsp)))
