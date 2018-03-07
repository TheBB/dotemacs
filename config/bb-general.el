(bb-package general
  :init
  (defmacro bb-leader (&rest args)
    (require 'general)
    `(general-define-key :prefix "SPC" :keymaps 'normal ,@args)))
