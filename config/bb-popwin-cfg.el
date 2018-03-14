(defmacro bb-popwin (mode &rest args)
  `(push '(,mode ,@args) popwin:special-display-config))
