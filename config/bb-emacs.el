(bb-package emacs
  :boot
  (put 'bb-autoload 'lisp-indent-function 1)
  (put 'bb-package 'lisp-indent-function 1)
  (put 'bb-leader 'lisp-indent-function 0)
  (put 'bb-mm-leader 'lisp-indent-function 1)

  :post-init general
  (bb-leader
    "<tab>" 'bb-alternate-buffer
    ";" 'eval-expression
    "bd" 'bb-kill-buffer
    "fd" 'bb-kill-buffer-file
    "fs" 'save-buffer
    "fy" 'bb-show-and-copy-filename
    "hc" 'bb-find-config
    "w" 'hydra-windows/body)

  :post-init popwin
  (bb-popwin help-mode))
