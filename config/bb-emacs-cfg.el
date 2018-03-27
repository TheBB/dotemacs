
;; Find files

(defun bb-find-config ()
  (interactive)
  (find-file (bb-dir "config.el")))

(defun bb-show-and-copy-filename ()
  (interactive)
  (when-let* ((file-name (or (buffer-file-name) list-buffers-directory)))
    (message (kill-new file-name))))


;; Window management hydra

(defun bb-shrink-window-horizontally (delta)
  (interactive "p")
  (shrink-window delta 'horizontal))

(defun bb-shrink-window-vertically (delta)
  (interactive "p")
  (shrink-window delta nil))

(defun bb-enlarge-window-horizontally (delta)
  (interactive "p")
  (enlarge-window delta 'horizontal))

(defun bb-enlarge-window-vertically (delta)
  (interactive "p")
  (enlarge-window delta nil))

(defhydra hydra-windows ()
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("s" evil-window-split)
  ("v" evil-window-vsplit)
  ("d" delete-window)
  ("(" bb-shrink-window-horizontally)
  ("[" bb-shrink-window-vertically)
  (")" bb-enlarge-window-horizontally)
  ("]" bb-enlarge-window-vertically)
  ("u" winner-undo)
  ("=" balance-windows-area :exit t))


;; Miscellaneous handy functions

(defun bb-kill-buffer ()
  (interactive)
  (kill-buffer nil))

(defun bb-kill-buffer-file ()
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (y-or-n-p "Are you sure you want to delete this file? ")
        (delete-file filename 'trash)
        (kill-buffer buffer)))))

(defun bb-alternate-buffer ()
  (interactive)
  (let ((buf (window-buffer)))
    (switch-to-buffer
     (cl-find-if (lambda (b) (not (eq b buf)))
                 (mapcar 'car (window-prev-buffers))))))
