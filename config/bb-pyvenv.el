(bb-package pyvenv
  :init
  (bb-mm-leader python-mode
    "va" pyvenv-workon
    "vd" pyvenv-deactivate)

  :post-init lsp-mode
  (add-hook 'pyvenv-post-activate-hooks 'lsp-restart-workspace)
  (add-hook 'pyvenv-post-deactivate-hooks 'lsp-restart-workspace))
