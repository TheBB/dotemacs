(bb-package flycheck

  :init
  (with-eval-after-load 'flycheck
    (aset flycheck-error-list-format 4 '("Message" 0 t)))

  :post-init lsp-mode
  (add-hook 'lsp-mode 'flycheck-mode))
