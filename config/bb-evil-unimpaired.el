(bb-package evil-unimpaired
  :init

  ;; Ignore useless buffers in `next-buffer', `previous-buffer', etc.
  (let ((entry (assq 'buffer-predicate default-frame-alist)))
    (if entry
        (setcdr entry 'bb-useful-buffer-p)
      (push '(buffer-predicate . bb-useful-buffer-p) default-frame-alist)))

  :post-init evil
  (with-eval-after-load 'evil
    (define-key evil-motion-state-map (kbd "[b") 'previous-buffer)
    (define-key evil-motion-state-map (kbd "]b") 'next-buffer)
    ))
