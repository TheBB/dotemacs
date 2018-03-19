(bb-package structured-editing
  :post-init smartparens
  (bb-mm-leader emacs-lisp-mode "l" 'hydra-structured-editing-lisp/body)
  (bb-mm-leader lisp-mode "l" 'hydra-structured-editing-lisp/body)
  (bb-mm-leader scheme-mode "l" 'hydra-structured-editing-lisp/body))
