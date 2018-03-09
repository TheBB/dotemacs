(bb-package python

  :post-init lsp-mode
  (lsp-define-stdio-client lsp-python "python"
    (lsp-make-traverser (lambda (dir)
                          (directory-files dir nil "\\(setup\\)\\.py")))
    '("pyls"))
  (add-hook 'python-mode-hook 'lsp-python-enable)

  :post-init company
  (bb-company python-mode company-capf))
