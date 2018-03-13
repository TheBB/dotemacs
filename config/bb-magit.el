(bb-package magit
  :init
  (with-eval-after-load 'with-editor-mode
    (diminish 'with-editor-mode))

  :post-init general
  (bb-leader "gs" 'magit-status)

  :post-init evil-unimpaired
  (push "magit.*" bb-useless-buffers-regexp))
