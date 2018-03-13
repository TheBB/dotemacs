(bb-package lsp-ui
  :init
  (autoload 'lsp-ui-imenu-enable "lsp-ui-imenu")
  (autoload 'lsp-ui-doc-enable "lsp-ui-doc")
  (autoload 'lsp-ui-sideline-enable "lsp-ui-sideline")
  (autoload 'lsp-ui-flycheck-enable "lsp-ui-flycheck")

  ;; Reasons for overriding the default `lsp-ui-flycheck-enable':
  ;; I don't want to see flycheck updating while editing. After save is
  ;; good enough.  However lsp-ui overrides
  ;; `flycheck-check-syntax-automatically' and calls `flycheck-buffer'
  ;; from its own hooks. Deleting this crap restores the functionality
  ;; that I want.
  (with-eval-after-load 'lsp-ui-flycheck
    (fset 'lsp-ui-flycheck-enable 'bb-lsp-ui-flycheck-enable))

  :post-init lsp-mode
  (add-hook 'lsp-mode-hook 'bb-lsp-enable-ui))
