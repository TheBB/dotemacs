(bb-package flycheck

  :init
  (bb-leader
    "tf" 'flycheck-mode
    "el" 'flycheck-list-errors)
  (setq-default flycheck-check-syntax-automatically '(save mode-enabled))
  (with-eval-after-load 'flycheck
    (diminish 'flycheck-mode "f")
    (aset flycheck-error-list-format 5 '("Message" 0 t)))

  :post-init popwin
  (bb-popwin flycheck-error-list-mode :noselect t)

  :post-init lsp-mode
  (add-hook 'lsp-mode-hook 'flycheck-mode))
