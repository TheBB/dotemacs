
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
