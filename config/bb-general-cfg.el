(defmacro bb-leader (&rest args)
  `(progn
     (require 'general)
     (general-define-key :prefix "SPC" :states '(normal motion) ,@args)))
