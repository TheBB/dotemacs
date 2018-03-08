(defmacro bb-leader (&rest args)
  (declare (indent 0))
  `(progn
     (require 'general)
     (general-define-key :prefix "SPC" :states '(normal motion) :keymaps 'override ,@args)))

(defmacro bb-create-dispatch (keys)
  (let ((funcname (intern (format "bb-dispatch-%s" keys)))
        (varname (intern (format "bb-dispatch-table-%s" keys))))
    `(progn
       (unless (boundp ',varname)
         (defvar ,varname nil)
         (defun ,funcname ()
           (interactive)
           (catch 'done
             (dolist (entry ,varname)
               (when (derived-mode-p (car entry))
                 (throw 'done (call-interactively (cdr entry)))))
             (user-error "No dispatch found for \"%s\" in %s" ,keys major-mode)))
         (bb-leader ,keys ',funcname)))))

(defmacro bb-mm-leader (mode &rest args)
  (declare (indent 1))
  (let (bindings)
    (while args
      (push (cons (car args) (cadr args)) bindings)
      (setq args (cddr args)))
    `(progn
       ,@(cl-loop for binding in bindings
                  collect `(bb-create-dispatch ,(car binding)))
       ,@(cl-loop for binding in bindings
                  collect `(push '(,mode . ,(cdr binding))
                                 ,(intern (format "bb-dispatch-table-%s" (car binding))))))))
