(eval-when-compile
  (require 'cl-lib)
  (require 'hydra))



;; Convenience functions for leader bindings

(declare-function 'general-define-key "general")

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
           (cl-loop for entry in ,varname
                    if (derived-mode-p (car entry))
                    return (call-interactively (cdr entry))
                    finally return (user-error "No dispatch found for \"%s\" in %s" ,keys major-mode)))
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
                  collect `(push (cons ',mode ,(cdr binding))
                                 ,(intern (format "bb-dispatch-table-%s" (car binding))))))))



;; Buffer predicate function

(defvar bb-useful-buffers-regexp nil)

(defvar bb-useless-buffers-regexp
  '("\\*Messages\\*"
    "\\*Help\\*"))

(defun bb-useful-buffer-p (buffer)
  "Determine if a buffer is useful."
  (let ((name (buffer-name buffer)))
    (or (with-current-buffer buffer
          (derived-mode-p 'comint-mode))
        (cl-loop for regexp in bb-useful-buffers-regexp
                 thereis (string-match-p regexp name))
        (cl-loop for regexp in bb-useless-buffers-regexp
                 never (string-match-p regexp name)))))



;; Evil-numbers

(defhydra hydra-numbers ()
  ("=" evil-numbers/inc-at-pt)
  ("+" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt)
  ("_" evil-numbers/dec-at-pt))



;; Evil-unimpaired

(defun bb-insert-line-above (count)
  (interactive "p")
  (dotimes (- count) (save-excursion (evil-insert-newline-above))))

(defun bb-insert-line-below (count)
  (interactive "p")
  (dotimes (- count) (save-excursion (evil-insert-newline-below))))

(defun bb-insert-spaces-before (count)
  (interactive "p")
  (dotimes (- count) (insert " ")))

(defun bb-insert-spaces-after (count)
  (interactive "p")
  (forward-char)
  (dotimes (- count) (insert " "))
  (backward-char (1+ count)))



;; Smartparens

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



;; Miscellaneous

(defvar popwin:special-display-config)

(defmacro bb-popwin (mode &rest args)
  `(push '(,mode ,@args) popwin:special-display-config))


(provide 'bb-defs)
