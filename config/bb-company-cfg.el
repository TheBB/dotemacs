(defvar bb-company-global-backends nil)

(defmacro bb-company (mode &rest backends)
  (let ((funcname (intern (format "bb-company-%s" mode)))
        (hookname (intern (format "%s-hook" mode))))
    `(progn
       (defun ,funcname ()
         (company-mode)
         (setq-local company-backends
                     (list (append ',backends bb-company-global-backends))))
       (add-hook ',hookname ',funcname))))
