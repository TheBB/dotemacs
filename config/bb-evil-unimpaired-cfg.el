
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



;; Text manipulation

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
