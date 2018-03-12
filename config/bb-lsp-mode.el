(bb-package lsp-mode
  :init
  (bb-leader "tl" 'lsp-mode)
  (require 'lsp-mode)
  (put 'lsp-define-stdio-client 'lisp-indent-function 2)
  (with-eval-after-load 'lsp-mode
    (diminish 'lsp-mode "l")))
