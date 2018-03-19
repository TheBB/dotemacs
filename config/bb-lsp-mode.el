(bb-package lsp-mode
  :init
  (bb-leader "tl" 'lsp-mode)
  (put 'lsp-define-stdio-client 'lisp-indent-function 2)
  (setq lsp-highlight-symbol-at-point nil)
  (require 'lsp-mode)
  (diminish 'lsp-mode "l"))
