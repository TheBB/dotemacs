(bb-package magit
  :post-init general
  (bb-leader "gs" 'magit-status)

  :post-init evil-unimpaired
  (push "magit.*" bb-useless-buffers-regexp))
