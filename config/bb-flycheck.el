(bb-package flycheck

  :init
  (bb-leader "tf" 'flycheck-mode)
  (with-eval-after-load 'flycheck
    (diminish 'flycheck-mode "f")
    (aset flycheck-error-list-format 4 '("Message" 0 t)))

  :post-init lsp-mode
  (add-hook 'lsp-mode-hook 'flycheck-mode))
