(defun startup-echo-area-message ()
  "Override startup message."
  "")


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
  ("]" bb-enlarge-window-vertically))


;; Miscellaneous handy functions

(defun bb-kill-buffer ()
  (interactive)
  (kill-buffer nil))
