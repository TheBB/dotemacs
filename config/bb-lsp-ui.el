(bb-package lsp-ui
  :init
  (autoload 'lsp-ui-imenu-enable "lsp-ui-imenu")
  (autoload 'lsp-ui-doc-enable "lsp-ui-doc")
  (autoload 'lsp-ui-sideline-enable "lsp-ui-sideline")
  (autoload 'lsp-ui-flycheck-enable "lsp-ui-flycheck")

  :post-init lsp-mode
  (add-hook 'lsp-mode-hook 'bb-lsp-enable-ui))
