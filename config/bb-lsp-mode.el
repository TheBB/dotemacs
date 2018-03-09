(bb-package lsp-mode
  :init
  (require 'lsp-mode)
  (put 'lsp-define-stdio-client 'lisp-indent-function 2))
