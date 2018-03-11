(bb-package flycheck
  :post-init lsp-mode
  (add-hook 'lsp-mode 'flycheck-mode))
